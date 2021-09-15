This document was started fresh to keep note of failed attempts as sanely implementing
an AST for STQL.


# High-level problems

## std::variant-based syntax tree (Scraped on 2021-03-15)

From the project inception, I was using `std::variant` to encode each type of AST. This
ran into an issue as:

1. We need to know the AST nodes before hand for checking for malformed expressions.
    - This led to issues with having to change many things in various locations whenever
      1 change needed to be made in a high-level.
2. I tried addressing the above issue using nested variants, but that quickly got
   annoying.

Proposed solution:

- Inheritance. :sigh: I will use runtime polymorphism to encode the rules of the
  STQL grammar.

# Syntactical issues with STQL

STQL formally doesn't have any notion of intervals, and these need to be inferred from
the constraints on the clock variables. The issue with this is that we 
