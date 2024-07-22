# Revision history for futhark-manifest

## 1.5.0.0

* Adding record arrays and opaque arrays.

* Added array indexing.

* Slight refactoring to emphasize that can opaque cannot be both e.g.
  a sum type and a record.

## 1.4.0.0

* Added `arrayNewRaw` and `arrayRaw`.

## 1.3.0.0

* Added `SumOps` and `SumVariant`.

* The `TypeOpaque` constructor now has an additional field of type
  `SumOps`.

## 1.2.0.1 -- 2023-03-10

* Fix test suite.

## 1.2.0.0 -- 2023-03-10

* Support for tuning parameters.

## 1.1.0.0 -- 2022-06-27

* Support records.

## 1.0.0.1 -- 2021-12-19

* First version. Released on an unsuspecting world.
