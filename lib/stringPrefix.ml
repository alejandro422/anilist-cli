let valueHasPrefix ~prefix value =
  String.length value > String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let valueWithoutPrefix ~prefix value =
  String.sub value (String.length prefix)
    (String.length value - String.length prefix)
