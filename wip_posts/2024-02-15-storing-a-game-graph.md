---
title: Storing a game graph using transposition tables
---

In a [recent post](), I talked about using Monte-Carlo tree search to play any game.
One key component that was missing from said post is *how to store information about
the game graph* **efficiently**.

This post expands on this question by presenting data structures you could *think*
to be efficient, before detailling how chameleon, my general game playing framework,
does it.

## The "simple" way: pointers galore
One could think about storing the graph as, well, *a graph*. The idea is that each
node, on top of the its information, stores a list of pointers to its children.

This works, and has the nice property that its easy to implement and does not require
any more work on the representation of our game itself. On top of that, we can access
children without playing moves.

However, it has some huge **drawbacks**:

- children are not necessarilly linked to moves. If you decide that the $n^{th}$ child is linked to the $n^{th}$ action for the current state, you break any implementation that does not guarantee that actions are always generated in the same order. If you use a hashmap from actions to pointers, you lose a lot of space.
- each node can become incredibly **heavy** with the added pointers. For Go, which has 19x19 possible actions in the initial state, that's **361 pointers**, or **2.888 kilobytes** on a 64bit machine.

If we want to store game graphs (which can contain millions of nodes), this becomes
a **huge** problem. 

On top of that, we keep some nodes which are not accessible anymore. Think about our
poor initial state, lost forever after the first move played, yet forced to live in agony
and solitude with its whopping 1.3 kilobyte node. You could potentially implement
some kind of garbage collector, but you're just adding insult to injury at this point.

## Hashing our way to victory
The efficient and practical way to store the game graph is to use a 
[transposition table](https://en.wikipedia.org/wiki/Transposition_table).

Transposition tables are hash tables specialized for games. The idea is to hash
game states, and use said hash to access information related to the state in a table.
**Sounds easy enough!**

Well, here's the bad news: game graphs are problematic because they are just **big**.
Storing an entire game graph in memory is, for most games, straight up impossible.
And finding a perfect hash function is just as impossible, since we simply don't have
enough values on 64 (or even 128) bits to create a one-to-one mapping. We'd need some 160 bits to even
hope mapping all chess positions, for example.

Even with 64 bit hash values, we cannot hope to store all of them in memory. This leads
us to our two main problems:

- two positions can have the same hash value (known as type-1 collisions).
- hash values can map to the same index in our table (known as type-2 collisions).

### Handling type-1 collisions is impossible
Our first problem is **non-solvable**. Therefore, it's not a problem! *Yay!*

I admit this is not satisfactory, so let's delve into it a little bit.

There are likely **more game states than there are 64 bit numbers**. This is true
for chess, go, and most games that are at least somewhat complex. So yeah, let's not
think about creating a perfect hashing, it will not work.

Now, can we at least **detect type-1 collisions**? Some of them, yes. But definitely not
every single one. You can add some verifier value, like the number of possible actions,
some of the possible actions, or even all of them. A little signature from the position
could help. But unless you store **all the game state** and use that as your verifier,
there is still *some way* that you'd encounter a type-1 collision.

Simply put, detecting type-1 collisions requires way too much memory to be efficient.
On top of that, it might not even be that much of a problem to begin with. Those
collisions are rare enough to not be encountered too often.

In the specific case of storing a Monte-Carlo search graph, type-1 collisions don't even
affect our results that much. Let's say two states map to the same key. They'll just **share a node**,
which will hold the mean of their individual results. 

- If both states are losing, well, the shared node will hold losing results as well. 
- If both are winning, the opposite happens!
- In the situation where the nodes have opposite values, they'll just be treated as a draw. You might miss a win, but at least you're not diving head first into a loss.

### Type-2 collisions, aka sorta free garbage collection
We cannot actually use 64 bit numbers as indices to our table. This would create a table
waaaaay too big for us to store in memory. What we'll have to do is **fix a table
size** which we'll be working with.

To find the index of our table a hash key corresponds to, we simply use `index = hash % size`{.rust}.

> One small optimization you should do is to **make the size a power of two**. The reason
> for this is that modulus by a power of two is equivalent to masking the $n^{th}$ lower
> bits of your hash, which is way faster than an actual modulus.
>
> This also enables us to save a bit more space. If we use the lowest part of our hash
> as a key, we simply need to store the rest of the bits to recreate our full key, instead
> of storing the key itself.

> How do we choose the right table size apart from that. Well, there's no right answer.
>
> A big table creates less collisions, but a smaller one might fit better into your cache
> line, making it way faster to access. **Just try stuff out and benchmark!**

This create a problem, however. Say two of our positions hash to `0xFF43453` and `0xAB53453`,
and we try to index a table of size $65536$. After applying our transformation, we get
two indices: `0x3453` and... `0x3453`.

This is easily detectable: just store the full hash key in the entry. It's not too big,
and serves as a reliable verifier. So, no problem?

Well, imagine that the first key maps to the initial position, and the second one
to a really, really important position, a pivotal state of the game, the grand moment!

Oh, but... the initial position is already stored here. Welp, just replace it, burn it down
and warm yourself by the fire.

But if we just always replace, the opposite scenario might happen! A really important result,
crushed and thrown into the trash by a random, unimportant result from a state that
is not even reachable anymore.

**That's it, we need a way to determine how important results are!**

So, what do we want?

- exact results are really important in Monte-Carlo Tree Search, we should try to keep them in memory, especially those inferred from their children.
- otherwise, we might prefer winning nodes to loosing ones.
- we should replace states which are no longer reachable, no matter how important their results are (we do not need them anyway).

Okay, the two first points are pretty straightforward: if there is a type-2 collision, we
inferred exact nodes are more important than those of final states, which themselves are more important than
any approximate result. If we need to choose between two approximate results, we pick
the one with the best winning rate.

> One **really** important thing to take into account (my smooth brain did not) is
> that **the root node should never, ever be replaced**.
>
> My first implementation ranked approximate nodes following their UCT score. The thing is
> that the root position **didn't necessarilly have the best one**!
>
> Instead, approximate nodes are now ranked based on how much they've been visited.
> This even makes it easier to apply penalties (we just divide the number of visits by
> a quarter).

What about replacing states which are no longer reachable?

The idea of chameleon is to use **penalties**. If you try to replace a node, but fail
to do so because it is better, the stored node receives a penalty. These penalties are
accumulated after each unsuccessful replacement, and reset if the node is accessed.

If the node is never accessed and a lot of other nodes try to replace it, it will get
easier and easier to replace. Decent nodes might take a few hits, while really good ones
will have more time before replacement.

## Parallel access to the table
Monte-Carlo tree search is an algorithm well suited for **parallelization**. Multiple
threads are used to search the graph in parallel, netting more simulations in the same
time span.

> Here, I'll talk about [tree parallelization](https://dke.maastrichtuniversity.nl/m.winands/documents/multithreadedMCTS2.pdf). 
> There are other parallelization schemes for MCTS, like [root or leaf](https://www.lamsade.dauphine.fr/~cazenave/papers/parallelUCT.pdf) 
> parallelizations.
>
> Root parallelization is very hungry in terms of memory, since creates a separate
> game graph for each thread, but is otherwise really efficient. There might be an idea
> of instead dividing the size of tables for each thread.
>
> Leaf parallelization does multiple simulations in parallel during the expansion
> phase, but does not increase the strength of the algorithm too much.
>
> Tree parallelization seems like a sweet spot, balancing a decent increase in playing
> strength and no additionnal memory footprint (for a lockless implementation).
>
> These results are taken from [this paper by Winands, Chaslot and Jaap van den Herik](https://dke.maastrichtuniversity.nl/m.winands/documents/multithreadedMCTS2.pdf).

To parallelize it efficiently, every thread needs access to the graph at the same time.
This creates the usual problems of concurrent access, notably the feared *data race*.

We could take the simple route and use mutual exclusion variables on our nodes. When
a thread visits a node, it locks it, and other threads wanting to access it need to
wait for it to finish.

However, we'll take inspiration from Robert Hyatt and implement a [lockless version](https://craftychess.com/hyatt/hashing.html).

Our problem, and the main reason we use locks, is that concurrent accesses to **the same node**
by **different threads** could lead to a corruption of said node. Let's look at an
example with two threads.


