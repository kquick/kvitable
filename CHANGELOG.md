# Revision history for KVITable

## next

* The `RenderConfig.sortKeyVals` changed from a `Boolean` to a `Maybe fn`, where
  the `fn` is a sorting function which is provided a `(rowKeyvals, colKeyvals)`
  tuple and should return that tuple, sorted.  The `sortNumericAlpha` function
  can be used to obtain the old 'True' behavior.

## 1.0 3.0 -- 2024-02-22

* Support GHC 9.8.
* Internal updates to confirm safety and avoid partial functions.

## 1.0.2.0 -- 2023-01-09

* Support GHC 9.4.

## 1.0.1.0 -- 2022-05-26

* Support GHC 9.2 [thanks to Ryan Scott].

## 1.0.0.0 -- 2021-01-30

* Initial version
