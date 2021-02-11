module PythagoreanTriples where
    -- Check whether three integers are Pythagorean triples.
    -- pythagoreanTriple 3 4 5
    -- pythagoreanTriple 5 3 4
    -- not (pythagoreanTriple 2 3 4)

    pythagoreanTriple a b c = (a^2 + b^2) == c^2