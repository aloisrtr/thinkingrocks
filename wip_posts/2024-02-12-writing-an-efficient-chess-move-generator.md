---
title: "Writing an efficient game representation for chess"
---

I've recently (re-)fell in the good old rabbit hole that got me into computer science
in the first place: chess programming.

This is probably my third or fourth rewrite of [chameleon-chess](https://github.com/aloisrtr/chameleon-chess),
except this time I'm pretty happy with the game representation, which was a huge blocking
point for me before. So, as always when I'm happy that something works, I'd like
to write about how!

<!--more-->

## What's a game representation? Why do we care?
Chess engines (and many board game bots) can be separated into three core components:

- game representation, responsible for encoding game state and actions from one state to another.
- search algorithms to find the best action given a game state.
- evaluation, heuristics to guess the probabilities of winning when we're in a certain state.

If you've got some knowledge about computer science, a game representation can be thought of
as a really really big graph where states are linked to each other if an action from state A can
transform it into state B.

> Games are often really, **really** big, too big to fit into memory, which is why we define the graph *implicitly*.

Game representation is a bit of a sore spot for me. To put it bluntly, it can break
**but not make** your engine. A bad game representation leads to really poor performance,
yet a good one does not make your engine inherently stronger. This imbalance is caused by the fact
that you rely on game representation to traverse game states, so it need to be
at least really really efficient. But you also want to avoid traversing the tree too much,
so the stronger your engine becomes, the less game representation speed is relevant.

Writing an efficient and correct game representation is **important**, and there
aren't a ton of detailled tutorials on how to do so. Let's fix that!

> The [chess programming wiki](https://www.chessprogramming.org) offers a lot of information on various techniques used, and in great detail!
>
> It just lacks the "put it all together" part.

## Ok, what do we need?
A good first question indeed: what information do we need to store?

A chess position is represented by:

- the position of pieces on the board.
- which player's turn it is.
- [castling rights](https://en.wikipedia.org/wiki/Castling).
- [en passant](https://en.wikipedia.org/wiki/En_passant) target square.

> There are also a few more metadata fields needed, like keeping track of [repetitions](https://en.wikipedia.org/wiki/Threefold_repetition) or the [fifty-move rule](https://en.wikipedia.org/wiki/Fifty-move_rule), but they're not that relevant here.

We also need to know **how** these informations will be used. Actions in chess are moves. We need
to be able to know which moves we can make, and how to modify the position's data to go
back and forth between positions given a move.

These will be our three main functions:

- `fn make(p: Position, m: Move)`{.rust} modifies the position as if we played the given move.
- `fn unmake(p: Position, m: Move)`{.rust} resets the position state to what it was before making a move.
- `fn moves(p: Position) -> [Move]`{.rust} creates a list of legal moves for a given position.

> We could choose to generate **pseudo-legal moves**, which follow the rules of chess, but might leave the king in check.
>
> Those moves are invalid, which means that we need to check whether they leave the king in check after making them instead of during move generation.
>
> Most of the time, this tradeoff is **not worth it**, so we'll generate only legal moves.

## Encoding our chess board
I'll be keeping this part brief: **try to compress your representation**.

Using the cache efficiently is incredibly important for chess engines.

For reference, piece codes are kept a byte in size, moves are two bytes.

As for the actual board, there are two ways to go about it: **piece** or **square** centric.
The former keeps information of where pieces are, while the latter
keeps information of what is on a given square.

![Diagram comparing piece and square centric representations](/files/writing-an-efficient-game-representation-for-chess/square_piece.png){ width=30% }

**Piece centric** representations are faster when iterating over pieces (in move generation
for example) because they avoid constantly checking "is there even a piece here?".
**Square centric** representations are faster at answering "which piece is on this square?",
which can be useful when making and unmaking moves.

Therefore, most engines nowadays say... **"why not both?"**. This wastes a bit of
space, and we get redundant information, but it's way faster to query said information
when needed!

My board representation combines an 8x8 array of `Option<PieceKind>`{.rust} with
a piece-centric representation known as [bitboards](https://en.wikipedia.org/wiki/Bitboard).

### Bitboards 101
A bitboard can be thought of as a set of elements that can be indexed, with the bit at
index $n$ set to $1$ if the $n^{th}$ element is present and $0$ otherwise.

The cool thing about applying this to chess boards is that **we have 64 squares**, and can
therefore store this set information in a **64 bit integer**.

The crux of the idea is to store one bitboard per piece kind and one bitboard per color.
We can then do setwise operations in **a single instruction**.

For example, say we want to get a bitboard of white pawns. Well, this translates to:
```rust
let white_pawn_bitboard = pawn_bitboard & white_bitboard
```
![Bitboard usage example](/files/writing-an-efficient-game-representation-for-chess/bitboard_example.png){ width=60% }

This becomes particularily useful when applying shifts, or filtering attackable/movable
squares, as we'll see in a minute.

## Generating moves
Now we move on to the interesting part: **how can we use that to generate moves?** I'll first present general techniques without caring about legality. We'll handle that afterwards!

### Pawns
Pawns are really benefitting from the bitboard representation. We start by filtering pawns
which are about to promote, separating them from the rest:
```rust
let pawns = pawn_bitboard & side_to_move_bitboard;
let promoting_pawns = pawns & promoting_rank;
let pawns = pawns ^ promoting_pawns;
```
We can than generate pawn moves by **shifting the entire bitboard** in different directions.

![We shift the bitboard in the north direction, moving all bits to the next rank. This represents our pawn pushes!](/files/writing-an-efficient-game-representation-for-chess/shift_example.png){ width=50% }

However, we're not done here: what if there are already pieces on the squares we want to
get to? Pawns can't capture by moving forward!

Well, we can mask this `pawn_pushes_bitboard` by an `empty` bitboard that contains
set bits wherever there are no pieces. This bitboard can be obtained by negating the union
of our two color bitboards, pretty neat!

![Taking an arbitraty empty bitboard, here's what would happen.](/files/writing-an-efficient-game-representation-for-chess/blockers_example.png){ width=60% }

I'm not going to detail more than this here, but the same ideas can be applied
for all pawn movements. You generate a set of potential target squares, then mask
it by squares that can actually be moved to (empty ones for pushes, enemy pieces for captures, etc).

### Knights and kings
Knights and kings are different from pawns: there are a looot of deltas that can be applied,
and most of the time only two knights and one king. We don't benefit much from parallelization
here.

Instead, we can calculate a lookup table of potential moves for each origin square!
We could then access the moves for a knight on d5 this way:
```rust
let knight_moves = KNIGHT_LOOKUP[Square::D5];
```

![Example of knight moves from the d5 square.](/files/writing-an-efficient-game-representation-for-chess/lookup_knight.png){ width=20% }

There's not much going on for knights and kings appart from that, they're pretty simple!

### Sliding pieces
*Uh oh...*

Sliding pieces (bishops, rooks and queens) are a bit more tricky. The main difficulty
is the fact that their moves can be thought of as rays that can be blocked by
other pieces! We can't simply create a lookup table associating an attack bitboard from
each square like knights and kings, since we also depend on the state of the board.

There are two (main) ways to go about things:

- calculating the attacks using fill algorithms on bitboards ([Dumb7Fill](https://www.chessprogramming.org/Dumb7Fill), [Kogge-Stone](https://www.chessprogramming.org/Kogge-Stone_Algorithm) or [Fill by subtraction](https://www.chessprogramming.org/Fill_by_Subtraction) are good examples).
- creating lookup tables indexed by square **and blockers**.

We'll be focusing on the latter since it is really often faster, albeit a bit more involved than
"filling until we hit a piece". More precisely, we'll be looking at [magic bitboard](https://www.chessprogramming.org/Magic_Bitboards).

> There are other ways to create lookup tables, which I'll mention briefly:
>
> - [rotated bitboards](https://www.chessprogramming.org/Rotated_Bitboards) cover line attacks only, and require additionnal bitboards updated incrementally to work efficiently.
> - [kindergarten bitboard](https://www.chessprogramming.org/Kindergarten_Bitboards) are more efficient, but require some calculation. They are a good middle ground between pure lookup and pure calculation!
>
> The main advantage of magic bitboards is that they cover rook and bishop attacks (two lines)
> at once rather than a single line each time.

What we're trying to find is a [perfect hash function](https://en.wikipedia.org/wiki/Perfect_hash_function)
from blockers to attacks for each square. If this seems like a complex endeavour, you'd
be right! Thankfully, smarter folks have already thought about it for us.

First of all, we don't want to perfectly hash *all possible blocker setups*. Taking a
bishop on e4, it doesn't matter to us wether a piece is on d2: we wouldn't cross it
anyway.

What we want is, once again, to **mask relevant blockers** and use this as a key. Already, we've
simplified our search quite a bit!

![Computing relevant blockers for a bishop on c4.](/files/writing-an-efficient-game-representation-for-chess/relevant_blockers.png){ width=60% }

Now, we've got a little problem... How do we transform our relevant blockers into a unique key?

Well, folks, I present to you the solution to any complex problem: **brute-force!**

Yup, you've heard that right, what we're going to find is a **magic number** (thus the name)
that, when multiplied with our bitboard and then shifted by a number of bits $n$,
perfectly maps blockers to the correct set of attacked square.

There is a [great article by Analog Hors](https://analog-hors.github.io/site/magic-bitboards/) on
the process of computing and using said magic numbers, so I won't delve into it too deeply.
Just remember that we have a function that, given an origin square and a set of blockers,
returns the squares attacked by a bishop/rook on said origin square.

> There are **a lot** of improvements for magic bitboards. Some are [dependant on
> specific hardware instructions](https://www.chessprogramming.org/BMI2#PEXTBitboards),
> others are implemented using more advanced hashing functions. 
>
> As mentionned before, small gains get more and more negligeable past a certain point.
> Magic bitboards are more than sufficient for now, and the small improvements granted
> by more advanced implementations tend to get negligeable within an engine, so I won't
> talk about them here.
>
> Still, it's a depply interesting ~rabbit hole~ topic to dive into if you've got
> time to dedicate to it! Maybe you could be the one to find better magic numbers
> for some origin square?

### "That was an illegal left by the way"
Okay, okay, let's make sure our moves are legal now!

First, let's see... Which of our generated moves *could be* illegal?
Well, there are a few:

- moves of a piece that would leave the king in check. Such pieces are called [absolutely pinned](https://en.wikipedia.org/wiki/Pin_(chess)) and cannot move except along the ray that pins them to the king.
- en passant captures can, in specific cases, leave the king in check by making two pawns disappear on the same rank.
- moves other than the king's if it is checked by two or more pieces.
- if the king is checked by a single piece, moves that do not capture the checking piece or block the attack ray.
- in general, king moves to attacked squares.
- castling over attacked squares.

*Phew*, okay, how do we deal with that?

#### Pins, checks and attacks
Three informations are clearly of importance here:
- whether the king is in check, and if so, does more than one piece attack it?
- which pieces are pinned, and where can they move?
- which squares are attacked?

This all depends on moves that the ennemy pieces can make! We don't want to generate
every move for every ennemy piece tho, that would just double our workload and is unnecessary.

What we'll do instead is compute three bitboards:
- attacked squares.
- checking piece(s).
- pieces of the moving side which are not pinned.

This is done by generating attack targets (not actual moves, the distinction is important)
for all ennemy pieces.

Pawns, knights and kings cannot create pins. This makes it easier to deal with them:
just generate their attack targets and add them to the bitboard of attacked squares.

If at this point the attack bitboard intersects with our king bitboard, we're in check!
We generate attacks from our king, and intersect them with ennemy pawns/knights, then add
the result to the checking pieces' bitboard.

> I've left out the ennemy king in that last part. The reason is that it cannot ever
> check our own king, otherwise we'd already be in an illegal position.

Now we get to the fun part: **sliders**.

First, the attack bitboard. One may think that simply reusing our magic bitboards would
work, but this leads to a sneaky bug: **our king is considered to be a blocker**.

![Example where using the king as a blocker is incorrect: we could generate a move to the north, which would still leave us in check.](/files/writing-an-efficient-game-representation-for-chess/slide_away.png){ width=50% }

> You might notice that some squares here are not *actually attacked*. So is our attack bitboard
> wrong?
>
> Well, if we're asking for a set of actually attacked squares, yes, it is wrong.
> But we're only interested in squares our king cannot move to **or** squares that we cannot
> castle through. Here, both are valid.

Our attack map is now correctly generated: yay!

What about pins tho? How do we deal with those?

What we'll do is generate sliding attacks **originating from our king** and **ignoring
allied pieces**, then intersecting this bitboard with corresponding ennemy sliders 
(bishops and queens for diagonal attacks, rooks and queens for orthogonal attacks).

If this intersection is not empty,
