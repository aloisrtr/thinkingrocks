---
title: "MCTS: how to play any game"
---

**Monte-Carlo Tree Search** (or MCTS) is a heuristic search algorithm used in general
game playing.

> This may sound like a pretty restricted use case, but in computer science games
> have a whole plethora of applications, and ~most~ **almost every** problem can
> be seen as a game!
>
> As a few examples, MCTS has been used in [folding proteins (Roucairol and Cazenave, 2023)](https://www.lamsade.dauphine.fr/~cazenave/papers/HP_MODEL_ICCCI.pdf),
> [physics simulation (Mao, Zhang, Xie and Basar, 2020)](https://proceedings.neurips.cc/paper/2020/file/30de24287a6d8f07b37c716ad51623a7-Paper.pdf),
> or [scheduling problems (Matsumoto, Hirosue, Itonaga, Yokoo and Futahashi)](https://www.iaeng.org/publication/IMECS2010/IMECS2010_pp2086-2091.pdf).
>
> Yup, the computer science definition of games **is amazing**! They're even more
> general than the famous [SAT problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem).

I'm in the process of writing [chameleon](https://github.com/aloisrtr/chameleon), 
a general game playing framework based on this very algorithm, so I figured I'd 
write a little post explaining how it works and how we can make it **play any game**.

## What's a game anyway
Mathematicians like formalism ~way too much for them to be sane~. The advantage is
that we don't have to debate over "is Animal Crossing **really** a game?" or stuff like that:
we know what a game is.

Games happen to have a lot of theory about them, which we'll refer to as the general field
of [game theory](https://en.wikipedia.org/wiki/Game_theory).

In this theory, a game is defined as a set of players $P$, where $p_1$ is the first player,
$p_2$ the second one, and so on and so forth. These players have a set of actions
that they can make, and utility functions associated to states of the game (win, loss, draw, etc).

> There are a lot of properties for games. A game can be:
>
> - symmetric if both players receive the same utility when playing the same set of actions.
> - zero-sum games are games where the sum of utilities for player is always equal to zero.
> - simultaneous games happen when both players pick their action at the same time (think of Pokémon battles).
> - perfect information games are some where every player knows every parameter about the game.
> - etc, etc, etc

You know what? Let's take an example! The good ol' rock-paper-scissors!

We have two players, $p_1$ and $p_2$, with both the same set of actions (rock, paper and scissors, no wells here).

Here's a way to represent this game, with the actions of $p_1$ in the rows and $p_2$ as the columns:

|              | **rock**  | **paper** | **scissors** |
|:------------:|:---------:|:---------:|:------------:|
| **rock**     | 0, 0      | -1, 1     | 1, -1        |
| **paper**    | 1, -1     | 0, 0      | -1, 1        |
| **scissors** | -1, 1     | 1, -1     | 0, 0         |

Cool, but this example lacks style. Both players choose at the same time (simulataneous),
and there is only one turn!

Instead, let's look at tic-tac-toe, a simple game that is representative of *most* board
games which [chameleon](https://github.com/aloisrtr/chameleon) will focus on.

We still have two players, but the set of actions changes a bit. First of, the players play
**sequentially** (in turns), and the action taken by a player affects the actions that
the following player can make.

![The action of the first player limits the actions of the second player!](/files/mcts/reducing_actions.png){ max-height=20em }

A better way to represent this game would be a [directed graph](https://en.wikipedia.org/wiki/Directed_graph),
where the vertices of the graph are states of the game, and there is an edge from state
$a$ to state $b$ if an action taken in $a$ leads to state $b$.

![A small subset of the tic-tac-toe game graph](/files/mcts/tic_tac_tree.png){ max-height=20em }

> Notice that I'm talking about a directed graph here and not a tree. Some states
> can repeat, be accessed through different sequence of actions, etc etc.
>
> This is just a more general way of looking at games! The only problem we *might* have
> for MCTS is that our game should not contain cycles. Otherwise, we might get stuck
> in an infinite loop!

> Also notice that the rock-paper-scissors example **can** be represented as a graph as well.
> The only difference is that it would make for a really boring graph.

The idea is that we start at an initial state $S$, and then play the game until we reach
a **final state** $F$. There can be many final states, each associated with a utility.

![Exemple of a final state (crosses win, because the opponent was probably sleeping)](/files/mcts/final_state.png){ max-height=20em }

Well, here, the circles lost... But come on, everyone knows that tic-tac-toe always ends up
in a draw if you're careful enough, right?

## How to win at games
Game theorists are really smart, and they figured out something: every player wants to win!

**But how?**

Well, they took inspiration from how we ourselves play games. Most often, you and I try
to "look ahead". "If my opponent does this, then I'll do that, and then...", you know, that
type of thinking.

Thankfully, we actually know how to program a computer to do that! There are many algorithms,
but the simplest one is [minimax](https://en.wikipedia.org/wiki/Minimax).

The idea is simple: for every action I can take, I check if its final.

- if it is, then I'll pick it if it means I win!
- if it is not, then I'll play the move in my head and repeat the process, but from my opponent's point of view (they want to win as well, you know).

Basically, you alternate between finding what's best for your opponent on his turn,
and what's best for you on your turn!

One important thing is that if all the actions you can take lead to final states, well you pick
the best outcome for you, and then say that this current state is final as well, with your choosen
outcome as its utility value.

If you keep going, then eventually every action from the initial state leads to a final
state. Just pick the best one!

## Yup, but games are like, big, right?
Well that's the problem. Tic-tac-toe's graph can be visited by a computer. But for more
intricate games such as chess, you've got more positions than atoms in the universe! How
are you going to visit that?

Obviously, we're not going to. Instead, we're once again going to inspire ourselves from
how humans think about games.

Top players for games like chess can look some 10-15 moves ahead. But they do it in a
smart way, discarding actions early when they "know" that its not going to have a good
outcome. They apply something we call [heuristics](https://en.wikipedia.org/wiki/Heuristic).

The idea is to "intelligently guess" the utility of a game state, without it being
a final state at all. For example, lets look at this game state in chess:

![]()

It's not final, yet we can still guess that the white side is in a bad position. 
It has less pieces (material), its king seems to be vulnerable to attacks, etc.

Applying those heuristics, we can avoid searching all of the game graph and just assign
approximate utilities for game states once we think we've searched far enough.

This approach has some problems, however:

- we could search a position up to a certain depth, assign it a good approximate utility, without knowing that if we searched just a little bit deeper we'd have found that this state is definitely loosing. This is known as the [**horizon effect**](https://en.wikipedia.org/wiki/Horizon_effect).
- our heuristics must be crafted with **domain-knowledge**. A good heuristic for tic-tac-toe will never translate to chess or go.

> There are a bunch of techniques to **mitigate** the horizon effect, like searching
> until a state seems "stable" (known as [quiescence search](https://en.wikipedia.org/wiki/Quiescence_search)), 
> or simulating a lot of random playouts (reaching a final state through playing random moves) 
> to check if on average we match our heuristic.
>
> Still, the horizon effect is the price to pay for using heuristics. We cannot get rid of it entirely.

This approach allows us to avoid searching at insane depths. However, we still need to find
a way to "discard" obviously bad moves early.

## Here comes ~sun~ MCTS, doo-doo-doo-doo
MCTS sort of combines all of those ideas. It does not require heuristics (altough it can
be more efficient when using them), and searches seemingly good moves more than seemingly
bad ones, instead of methodically searching the whole game graph.

The idea came from statistics. "Monte-Carlo" is the name of a city where you can find
a really prestigious casino. Let's say you're feeling lucky and try to play at the slot machine.

You've got a number of arms you can pull, and you either lose or win with a certain probability $P(&_n)$ for
each arm $n$. You have no idea which arms are good or bad!

Okay, back to games. You have a state with $n$ actions $a_1, ..., a_n$, and no idea
which of them are winning or losing. Wait... isn't that the exact same problem?

> On a small tangeant: this is a really neat thing in computer science. When you
> formalize problems correctly, you can find ways to transform some problems you don't
> know how to solve into other problems which you can solve instead.
>
> This is known as [reaductions](https://en.wikipedia.org/wiki/Reduction_(complexity))
> and is a pretty beautiful idea in my opinion.
>
> Note that what we're doing here is not *really* a reduction, we're just expressing
> our problem differently. I just wanted to show off this cool part of my research field!

### But how do we know which arm to choose?
That's the crux of MCTS: choosing the best arm (probably).

One intuition we might have to "guess" the winning probabilities for each action is
to experiment, or **explore**. Pick a random action, then find a final state by playing
randomly. Is it good? Okay, this action nets us a win 100% of the time over 1 experiment. Cool!

But you'll agree that 1 experiment is far from enough. Thankfully, we're talking about computers,
they can do the repetitive work of conducting millions of tests for us!

The results of these experiments are reported in a **Monte-Carlo graph**. It is a subset
of our game graph, but with added approximated scores and number of experiments for each state.

But if we're just experimenting randomly, well, we're losing a lot of time on
bad arms. Back to our casino example, this would mean losing a lot of money! We don't want that.

What we want is to find a good balance between **exploration** (checking out actions we
maybe haven't tried enough) and **exploitation** (searching actions that are likely
to be good more deeply).

To do this, MCTS is separated in **4 phases**:

- **selection** is the act of choosing one non-visited state. For each state starting from our initial state, we select an action based on its exploration/exploitation value, until we find an action that we haven't tested yet.
- at this point, we **expand** the action. We play it, create a node for the resulting game state, and set the number of experiments from this node to 1.
- but wait, we haven't experimented yet! Let's fix that by **simulating** a random game.
- the result of this random game is then **backpropagated** to all nodes we traversed. Basically, we report the result of our little experiment!

By doing this enough times, we are really likely to explore good actions deeper, without
straight up not exploring actions we think to be bad from a small number of experiments.

> Notice that we **don't mention any hand-made heuristic function**. We don't need one!
> All that MCTS needs is a way to go from state to state, utility values for final states,
> and we're done! We can have really good results *without* having to search the entire graph.
>
> We also diminish the horizon effect, altough we're now more subject to "bad luck" if
> our random experiments fail to see a really good or terrible action.

## Implementing Monte-Carlo Tree Search for any game
Let's summarize what we need to represent a game:

- a way to represent any game state.
- given a game state, what actions can we take?
- going from one game state to another (in **both ways**, applying and undoing an action).
- assigning utility values to final states.

Those constraints will be represented by a Rust trait aptly called `GameState`{.rust}. Anything
implementing this trait can be used in a Monte-Carlo Tree Search.

As a simple example, we'll go back to tic-tac-toe:

- the game state is represented as a 3x3 array of either `Empty`{.rust}, `Cross`{.rust} or `Circle`{.rust}.
- actions are marking any `Empty`{.rust} cell with the current player's mark.
- you can just write the player's mark to the selected cell, and store this choice in a stack.
- final states can be either drawn, won by `Cross`{.rust} or won by `Circle`{.rust}.

Great, we've got a simple tic-tac-toe game that we can play. Let's make a bot out of it.

To keep things simple, we'll use the most common exploitation/exploration formula: [**U**pper **C**onfidence Bound1 applied to **T**rees (UCT)].

$\frac{w_i}{v_i} + c \sqrt{\frac{{ln} V_i}{v_i}}$

- $w_i$ is the number of winning simulations for state $i$.
- $v_i$ is the number of visits to a state $i$.
- $c$ is the exploration parameter (how much do we want to explore?).
- $V_i$ is the number of visits to the parent state of $i$.

During the **selection** phase, we'll always take the action that maximizes this
formula, play it, then keep going until we find a node that hasn't been visited.

Once we found a non-expanded node, well, we expand it of course! It can either be assigned
an `Exact(...)`{.rust} score if it is a final state, or an `Approximate(visits, score)`{.rust} otherwise.

`Approximate(visits, score)`{.rust} utilities are initialized by doing a simulation of
a random game from our expanded state, so a newly expanded node will always have a value of either
`Exact(win/lose)`{.rust} or `Approximate(1, simulated_score)`{.rust}.

Once this is done, we need to update every state we've visited up until that point.
Simply undo the last action, add a visit and the simulated score, then go up once again, 
until we are back at the root.

> We need to be careful about **how we handle our utility**. Since we're switching between
> players at each move, a win for `Circle`{.rust} is a loss for `Cross`{.rust} and vice-versa.
>
> When updating our values, we must be careful to invert the value from the child node.
>
> The way this is handled in chameleon is by abstracting utility values so that they can
> be accessed by players. If we access a utility using `Circle`{.rust} it will return us
> the value from its perspective.

If we repeat this a bunch of times, our MCTS graph will have a bunch of information
about which actions are best for our initial state! If we have to choose the best,
all we need is to select the one that leads to the child with the highest visit count.

And **boom**! We've got ourselves an algorithm that can play any game. We used tic-tac-toe for
our exemple values here, but we simply need to replace them with values adapted to chess,
go, Rubik's cube, or protein folding to be able to play those "games" as well.

> A big question is "how do we store the MCTS graph?".
>
> I'll leave that to the reader, but the way chameleon does it is using a 
> [transposition table](https://en.wikipedia.org/wiki/Transposition_table).
>
> These are basically hash tables for game states. The main drawback is that it requires
> games to implement a hash function for their states, but in return we get implicit
> edges (to see the children of a position, just play the actions and access the 
> entries using the hash of the new state) and free handling of transpositions
> (positions that appear from a different sequence of moves).
>
> How this is implemented efficiently will be the subject of a future post, since
> they are quite fascinating data structures.

## A practical example: chess bot
Sooo, tic-tac-toe is great and all... but isn't that the *easy mode* of general game playing?
I mean, the game graph is small, and even humans can find the optimal move. It's completely solved,
we know it's a draw if both players play perfectly!

The final little test is to plug [chameleon-chess](), a chess game representation which
I talked about in my [last post](/posts/2024-02-12-writing-an-efficient-chess-move-generator.html),
to make a bot that actually plays chess (probably poorly... **for now**).

Let's go through the same process as defining tic-tac-toe to work with MCTS:

- our game state is, well, a `Position`{.rust}
- the actions that can be played are given using the `moves()`{.rust} function, which returns all legal moves for a chess position.
- we have our `make()`{.rust} and `unmake()`{.rust} functions to play/undo moves.
- oh no! We haven't yet described our utility!

The utility part is pretty straightforward tho. If `moves()`{.rust} returns an empty list,
the game is over. Either the side to move is in checkmate (loss), or a stalemate (draw).

> There are a few other cases due to FIDE rules, notably [draw by repetition](https://en.wikipedia.org/wiki/Threefold_repetition)
> or the [fifty move rule](https://en.wikipedia.org/wiki/Fifty-move_rule).
>
> Both are situations where a player can *claim a draw*, so we'll just detect them
> and say that they're drawn.

So, here's what our implementation of the `GameState`{.rust} trait would look like:
```rust
impl GameState for Position {
  fn play(&mut self, action: Self::Action) {
    self.make(action)
  }
  fn undo(&mut self) {
    self.unmake()
  }

  fn actions(&self) -> Self::ActionIter {
    self.moves()
  }

  fn utility(&self) -> Utility<Self::UtilityValue> {
    if self.is_checkmate() { 
      Utility::Exact(Self::UtilityValue::Loss(self.current_player())) 
    } else if self.is_draw() { 
      // Covers for stalemate, repetitions and fifty-move rule
      Utility::Exact(Self::UtilityValue::Draw) 
    } else {
      // This position is not a final state
      Utility::Unknown
    }
  }

  fn current_player(&self) -> Self::Player {
    self.black_to_move
  }
  fn hash(&self) -> u64 {
    self.hash
  }
}
```

As you can see, most of this implementation comes straight from the game representation
itself, not much more work needs to be done!

I'm pretty bad at chess, so for obvious reasons it destroyed me. To give a bit more
credit to the fact that it **can play chess** half-decently, I made a friend of mine
play against it. The game in question is right below:


It won!

## Closing thoughts
This is not the end of the journey. Our MCTS algorithm can be made much, **much** stronger
than it currently is.

The main way to make it better is to implement some [parallelization scheme](https://en.wikipedia.org/wiki/Parallel_computing)
so that we can multiply the number of searched states for a given time window.
This is a bit of a newly dealt with topic in game theory, so it's going to be fun!

There are also better exploration/exploitation balance functions that I could delve into,
such as [RAVE](https://web.archive.org/web/20170828123413/http://www.machinelearning.org/proceedings/icml2007/papers/387.pdf) 
or [Kullback-Leibler](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence).

Another idea is to use domain-specific knowledge that can be made available to assign
a bias to actions and skip simulations entirely, likely using neural networks and 
more precisely [NNUE](https://en.wikipedia.org/wiki/Efficiently_updatable_neural_network),
but this won't be done in the near future.

In the meantime, the code for [chameleon can be found here](https://github.com/aloisrtr/chameleon). 
It is also published as a Rust crate, so that you can simply declare it as a dependency 
to your game bot project, implement a game representation, and plug it straight into chameleon!

<p class="head center">See ya!</p>
