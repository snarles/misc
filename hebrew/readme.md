### Word Chaos Rules
---
**Overview**

Word Chaos is a mechanic to determine the winner between two words of equal length by comparing paired characters with special rules.

---
### Step 1: Equalizing Word Lengths

If words differ in length, extend the shorter word by repeating and truncating characters to match the longer word’s length.

---
### Step 2: Pairing Characters

Pair the characters from each word by their position.

---
### Step 3: Counting Reversals (Proximity)

For each character pair, check if the first word’s character is proximal to the second word’s character. The number of proximal pairs is the number of reversals.

#### Proximity Cases
* **Letter vs. Letter:** Letters are proximal if the first word’s letter is the same, or within the four letters before or after the second word’s letter. The alphabet wraps around (A↔Z), and case is ignored.
* **Letter vs. Digit:** Convert the digit to a letter using the following conversions:
    * 0 → O
    * 1 → I
    * 2 → Z
    * 3 → E
    * 4 → A
    * 5 → S
    * 6 → G
    * 7 → T
    * 8 → B
    * 9 → J
    Then, apply the Letter vs. Letter proximity rule to the letter and the converted digit-letter.
* **Digit vs. Digit:** Digits are proximal if the first digit is the same, the next digit, or the previous digit (with wraparound: 0 after 9).

---
### Step 4: Determine Conventional Outcome

Count how many pairs are won by the first word versus the second word using the Character Comparison rules from Step 5.
* If one word wins more pairs, it is the **conventional winner**.
* If there is a tie in pair wins, the winner of the first non-tied pair decides the outcome.
* If all pairs are tied, the conventional outcome is a tie.

---
### Step 5: Character Comparison

The type of comparison depends on the characters in the pair.
* If at least one character in a pair is a digit, **Shape Classification** applies.
* Otherwise, **Sound Classification** applies.

#### Shape Classification
* **Classes:**
    * **Straight:** 1, 4, 7, A, E, F, H, I, K, L, M, N, T, V, W, X, Y, Z
    * **Curved:** 2, 3, 5, C, G, J, S, U
    * **Loopy:** 0, 6, 8, 9, B, D, O, P, Q, R
* **Hierarchy:** The hierarchy is cyclic: **Straight > Loopy > Curved > Straight**.
* If characters are in different classes, outclassing determines the winner.

#### Sound Classification
* **Letters only:**
    * **Vowels:** A, E, I, O, U, Y
    * **Hard Consonants:** B, C, D, G, K, P, Q, T
    * **Soft Consonants:** F, H, J, L, M, N, R, S, V, W, X, Z
* **Hierarchy:** The hierarchy is cyclic: **Vowels > Hard > Soft > Vowels**.
* If letters are in different classes, outclassing determines the winner.

#### Outranking (Same Class)
* Letters always outrank digits.
* Among letters of the same class, the later alphabet letter wins.
* Among digits of the same class, the higher digit wins.
* Identical characters result in a tie for that pair.

---
### Step 6: Final Winner with Reversals

The final winner is determined by the conventional outcome and the reversal count.
* **Even number of reversals (including zero):** The overall winner is the conventional winner.
* **Odd number of reversals:** The overall outcome is reversed (winner ↔ loser; a tie remains a tie).

---

### Examples

F35 vs DR1

|Battle | F35| DR1 | win by | reversal check  | reversal? |
|-|-|-|-|-|-|
|Pair 1 | F | D | | F, D | yes (DEF) |
|winner | | X | outclass (sound) | | |
|Pair 2 | 3 | R | | E, R | no (EFGHI...R)|
|winner | | X | outclass (shape) | | |
|Pair 2 | 5 | 1 | | 5, 1 | no (12...5)|
|winner | X |  | outclass (shape) | | |
| Conventional winner | | DR1 | 2-1 | | |
| Final winner | F35 | | | 1 reversal | yes (odd) |

MCE vs MEN

|Battle | MCE | MEN | win by | reversal check  | reversal? |
|-|-|-|-|-|-|
|Pair 1 | M | M | | M, M | yes (M) |
|winner | |  | tied | | |
|Pair 2 | C | E | | C, E | yes (CDEF) |
|winner | | X | outclass (sound) | | |
|Pair 2 | E | N | | E, N | no (EFGHI...L)|
|winner |  | X | outclass (sound) | | |
| Conventional winner | | MEN | 2-1 | | |
| Final winner |  | MEN | | 2 reversals | no (even) |

0AB vs 94X

|Battle | 0AB | 94X | win by | reversal check  | reversal? |
|-|-|-|-|-|-|
|Pair 1 | 0 | 9 | | 0, 9 | yes (90) |
|winner | | X | outrank (loop) | | |
|Pair 2 | A | 4 | | A, A | yes (A) |
|winner | X |  | outrank (straight) | | |
|Pair 2 | B | X | | B, X | yes (XYZAB)|
|winner | X |  | outclass (sound) | | |
| Conventional winner | 0AB |  | 2-1 | | |
| Final winner |  | 94X | | 3 reversals | yes (odd) |
