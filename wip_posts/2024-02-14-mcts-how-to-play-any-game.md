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
> Yup, the computer science definition of games **is amazing**!

I'm in the process of writing [chameleon](), a general game playing framework based
on this very algorithm, so I figured I'd write a little post explaining how it works
and how we can make it **play any game**.

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

| | rock | paper | scissors |
|---|---|---|---|
| rock | 0, 0 | -1, 1 | 1, -1 |
| paper | 1, -1 | 0, 0 | -1, 1 |
| scissors | -1, 1 | 1, -1 | 0, 0 |

Cool, but this example lacks style. Both players choose at the same time (simulataneous),
and there is only one turn!

Instead, let's look at tic-tac-toe, a simple game that is representative of *most* board
games which [chameleon]() will focus on.

We still have two players, but the set of actions changes a bit. First of, the players play
**sequentially** (in turns), and the action taken by a player affects the actions that
the following player can make.

![The action of the first player limits the actions of the second player!](files/mcts/reducing_actions.png)

A better way to represent this game would be a [directed graph](https://en.wikipedia.org/wiki/Directed_graph),
where the vertices of the graph are states of the game, and there is an edge from state
$a$ to state $b$ if an action taken in $a$ leads to state $b$.

![A small subset of the tic-tac-toe game graph](files/mcts/tic_tac_tree.png)

> Notice that I'm talking about a directed graph here and not a tree. Some states
> can repeat, be accessed through different sequence of actions, etc etc.
>
> This is just a more general way of looking at games!

> Also notice that the rock-paper-scissors example **can** be represented as a graph as well.
> The only difference is that it would make for a really boring graph.

The idea is that we start at an initial state $S$, and then play the game until we reach
a **final state** $F$. There can be many final states, each associated with a utility.

![Exemple of a final state (crosses win, because the opponent was probably sleeping)](files/mcts/tic_tac_tree.png)

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
