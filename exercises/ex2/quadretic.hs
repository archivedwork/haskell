module Quadretic where
    -- quadratic 1.0 (-6.0) 8.0 == (2.0, 4.0)

    -- quadratic :: (Real b, Real b) => b -> b -> b -> (b, b)
    quadratic x y z = ( (-y) + y^2 - sqrt 4*x*z `div` 2*x, (-y) + y^2 + sqrt 4*x*z `div` 2*x )