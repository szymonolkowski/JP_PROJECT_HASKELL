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

**Złożoność:**

* Czas: `O(3^10 * 10 * n)` (praktycznie kilka milionów operacji — akceptowalne).
* Pamięć: `O(3^10 * n)`.

---
