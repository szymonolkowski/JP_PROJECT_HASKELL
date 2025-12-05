# JP HASKELL

---

## Spis treści

1. [Trójki pitagorejskie (zadanie 4)](#trójki-pitagorejskie-zadanie-4)
2. [Liczby niemonotoniczne (zadanie 24)](#liczby-niemonotoniczne-zadanie-24)
3. [Podwójne pandigitale / n](#podwójne-pandigitale--n)

---

# Trójki pitagorejskie (zadanie 4)

**Treść zadania (skrócona):**
Dla podanej liczby `n` znajdź, jeśli to możliwe, *pierwotną* trójkę pitagorejską (a, b, c) taką, że `a + b + c = n`. Pierwotna trójka pitagorejska to taka, której a, b, c nie mają wspólnego dzielnika > 1. Przykład: dla `n = 12` trójką jest `(3, 4, 5)`.

Jeżeli nie istnieje pierwotna trójka sumująca się do `n`, zwróć trójkę pierwotną dla największego możliwego `m` takiego, że `m < n`.

**Wejście:** pojedyncza liczba całkowita `n` (np. `n > 0`).

**Wyjście:** trójka `(a, b, c)` lub informacja o braku (według specyfikacji — zamiast braku zwracamy trójkę dla największego `m < n`).

**Przykład:**

* Wejście: `12` → Wyjście: `(3, 4, 5)`.

**Sugestia rozwiązania (Euclid):**
Użyj wzoru Euklidesa dla *pierwotnych* trójek:

* Niech `u > v > 0` całkowite, `gcd(u, v) = 1` i `u` i `v` mają różne parzystości (czyli nie są oboje parzyste ani oboje nieparzyste).
* Wtedy `a = u^2 - v^2`, `b = 2*u*v`, `c = u^2 + v^2` daje pierwotną trójkę.
* Suma: `S = a + b + c = 2*u*(u + v)`.

Zatem aby znaleźć pierwotną trójkę o sumie `n` musimy znaleźć pary `(u, v)` spełniające `2*u*(u + v) = n` z warunkami na gcd i parzystość. Jeśli nie ma rozwiązania dla danego `n`, zmniejszamy `m = n-1, n-2, ...` aż znajdziemy pierwsze `m` z rozwiązaniem.

**Złożoność:**

* W najgorszym wypadku szukamy kolejnych `m < n`. Możemy jednak faktoryzować `n/2` i testować dzielniki `u` szybciej. Znalezienie pary `(u, v)` wymaga iteracji po dzielnikach `n/2` lub po możliwych `u` do `O(sqrt(n))`.

---

# Liczby niemonotoniczne (zadanie 24)

**Treść zadania (skrócona):**
Dla danego prawdopodobieństwa `p`, `0 ≤ p ≤ 0.99`, znajdź najmniejsze `n` takie, że `p` jest mniejsze niż odsetek liczb niemonotonicznych w zbiorze całkowitych od `0` do `n`.

Definicje:

* Liczba jest *monotoniczna* jeśli jej cyfry są albo niemalejące (każda następna >= poprzedniej) albo nierosnące (każda następna <= poprzedniej).
* W przeciwnym razie liczba jest *niemonotoniczna*.

Przykłady monotonicznych: `222`, `1334578`, `9962`.
Przykłady niemonotonicznych: `253`, `2286`, `888878`.

Dodatkowa uwaga: dla małych `m` (np. `m ≤ 100`) odsetek może być 0 (w praktyce: dla bardzo małych przedziałów jest mało niemonotonicznych liczb).

**Wejście:** wartość `p` (rzeczowa, `0 ≤ p ≤ 0.99`).

**Wyjście:** najmniejsze `n` które spełnia warunek (tj. odsetek liczb niemonotonicznych w `0..n` jest > `p`).

**Sugestie rozwiązania:**

* Zliczenie liczb monotonicznych do `n` można zrealizować za pomocą *digit DP* (dynamic programming na cyfrach), ponieważ bezpośrednie przeglądanie wszystkich liczb jest niepraktyczne dla dużego `n`.
* Alternatywnie, dla całych zakresów długości cyfr można wykorzystać kombinatorykę:

  * Liczba całkowita o stałej długości `k` (z dopuszczonymi zerami na przodzie) jest niemalejąca wtedy i tylko wtedy, gdy jej cyfry tworzą nieściągalny ciąg kombinatoryczny. Liczbę niemalejących ciągów długości `k` z alfabetu `0..9` można policzyć jako `C(k+9, 9)` (ze względu na kombinacje z powtórzeniami). Podobnie da się policzyć ciągi nierosnące.
  * Uwaga: trzeba ostrożnie traktować liczenia z powodu powtórnego zliczania liczb stałych (np. same zera) i kwestii prowadzących zer.
* Najbezpieczniejszym i prostym do implementacji podejściem jest *digit DP* z parametrami: pozycja, poprzednia cyfra, stan czy dotychczas jest niemalejąco / nierosnąco / obydwa, flaga ograniczenia (czy dotychczas zapisujemy prefiks równy prefiksowi n), i flaga prowadzących zer.

**Cel:** znaleźć najmniejsze `n` takie, że:

```
(num_niemonotonicznych_do_n) / (n+1) > p
```

gdzie `num_niemonotonicznych_do_n = (n+1) - num_monotonicznych_do_n`.

**Złożoność:**

* Digit DP wymaga `O(d * states)` gdzie `d` to liczba cyfr (≈ log10(n)), a `states` to liczba możliwych poprzednich cyfr × stanów monotoniczności × flag ograniczeń; w praktyce jest szybkie (kilkaset/kilka tysięcy stanów) i wygodne.

---

# Podwójne pandigitale / n

**Treść zadania (skrócona):**
Podwójny pandigital to liczba składająca się z cyfr `0..9` każdej dokładnie **2 razy** (czyli 20-cyfrowa liczba), a `0` nie może być na najstarszej pozycji. Dla danej liczby `n` (`n ≤ 11`) policz ile istnieje takich podwójnych pandigitali, które są podzielne przez `n`.

**Wejście:** liczba całkowita `n` (`1 ≤ n ≤ 11`).

**Wyjście:** liczba sposobów (permutacji cyfr spełniających warunki) podzielnych przez `n`.

**Sugestia rozwiązania:**

* Problem polega na policzeniu permutacji 20 pozycji, w których każda cyfra 0..9 występuje dokładnie dwa razy, przy czym pierwsza cyfra ≠ 0, i liczba powstała jest podzielna przez `n`.
* Najprostsze/praktyczne podejście: *dynamic programming po liczbie stanów reprezentujących użycie cyfr* (DP na kombina­torialnym stanie):

  * Reprezentujemy ile razy użyliśmy każdej z 10 cyfr (każda 0..2) — to daje `3^10 = 59,049` możliwych stanów.
  * Dla każdego takiego stanu przechowujemy liczbę sposobów (liczb porządkowych) prowadzących do reszty modulo `n`. DP przechodzi dodając jedną kolejną cyfrę `d` (o ile jej użycie nie przekracza 2) i aktualizując resztę: `new_rem = (old_rem * 10 + d) % n`.
  * Trzeba też rozróżnić pozycję pierwszej cyfry: nie wolno stawiać `0` jako pierwszej — można to załatwić przez inicjalizację DP tylko z dopuszczalnymi cyframi na pierwszym kroku lub przez osobną obsługę pozycji 0.
  * Całkowita złożoność to `O(3^10 * 10 * n)` przy memoizacji/tabulacji, co jest wykonalne w czasie (przy `n ≤ 11`).
* Dodatkowe optymalizacje:

  * Szybkie kodowanie stanu wektorowego do liczby w systemie trójkowym.
  * Wykorzystanie faktu, że cyfry powtarzają się dokładnie dwa razy — można też liczyć permutacje z współczynnikami dwumianowymi przy brute-force dla reszt modulo n, lecz DP jest prostsze w implementacji i mniej podatny na błędy.

**Złożoność:**

* Czas: `O(3^10 * 10 * n)` (praktycznie kilka milionów operacji — akceptowalne).
* Pamięć: `O(3^10 * n)`.

---
