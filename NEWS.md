# PlaneGeometry 1.6.0 (2023-08-07)

- New functions `LineFromInterceptAndSlope` and `LineFromEquation`.

- New R6 class `Hyperbola`.


# PlaneGeometry 1.5.1 (2023-06-23)

- Unit tests and vignette chunks are now skipped if dependencies are missing.


# PlaneGeometry 1.5.0 (2023-06-17)

- The `path` method of the `Ellipse` class gains an argument `outer` allowing 
to choose whether the path approximating the ellipse will be interior or 
exterior (thanks to Duncan Murdoch for the help).

- New method `perimeter` for the `Ellipse` class; it returns the perimeter 
of the ellipse.

- New methods for the `Triangle` class: `perimeter` and `outerSoddyCircle` 
(beware - this circle possibly has a negative radius).

- New example in the vignette.


# PlaneGeometry 1.4.0 (2023-05-19)

- New function `maxAreaInscribedCircle`, to compute the circle inscribed in a 
convex polygon with maximum area. This optimization problem is solved with 
the help of the **CVXR** package.

- New function `maxAreaInscribedEllipse`, to compute the ellipse inscribed in a 
convex polygon with maximum area. This optimization problem is solved with 
the help of the **CVXR** package.


# PlaneGeometry 1.3.1 (2022-01-13)

New function `fitEllipse`, to fit an ellipse to a set of points. It uses the 
**fitConic** package.


# PlaneGeometry 1.3.0 (2021-12-21)

* Fixed the `isEqual` method of the `Line` class.

* Fixed Möbius raised at the power 0.

* Added the method `fixedPoints` to the `Mobius` class.

* New function `MobiusMappingCircle`, which returns a Möbius transformation 
mapping a given circle to another given circle.

* New function `MobiusSwappingTwoPoints`, returning a Möbius transformation 
swapping two given points.

* New function `EllipseFromThreeBoundaryPoints`, returning the smallest area 
ellipse passing through three given boundary points.

* New function `EllipseFromFociAndOnePoint`, returning the ellipse with given 
foci and a given point on its boundary.

* New function attached to the `Triangle` class: `MandartInellipse`, which 
returns the Mandart inellipse of the triangle.

* New function attached to the `Triangle` class: `hexylTriangle`, which 
returns the hexyl triangle of the triangle.

* New function attached to the `Triangle` class: `isogonalConjugate`, which 
returns the isogonal conjugate of a point with respect to the triangle.

* New examples in the vignette: an illustration of inversions, Schottky circles, 
modular tessellation, Apollonian gasket, and Malfatti gasket.


# PlaneGeometry 1.2.0 (2020-08-06)

* New methods for `Ellipse` class: `theta2t`, `pointFromEccentricAngle`, 
`normal`.

* New example in the vignette, the elliptical billiard.


# PlaneGeometry 1.1.0 (2020-02-24)

* Bug fixed: the function `inversionSwappingTwoCircles` did not work for all cases.

* New functions: `inversionFromCircle`, `midCircles`, `CircleAB`.

* New methods for `Circle` class: `tangentsThroughExternalPoint`, 
`isOrthogonal`, `angle`.

* New methods for `Triangle` class: `symmedialTriangle`, `symmedianPoint`, 
`BrocardCircle`, `BrocardPoints`, `LemoineCircleI/II/III`, `LemoineTriangle`, 
`ParryCircle`, `SteinerEllipse`, `SteinerInellipse`, `pointToTrilinear`, 
`pedalTriangle`, `CevianTriangle`.

* New methods for `Line` class: `distance`, `parallel`.

* New methods for `Mobius` class: `power`, `gpower`.


# PlaneGeometry 1.0.0 (2020-02-01)

First release.
