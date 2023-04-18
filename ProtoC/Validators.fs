module Validators

open Proto3

/// <summary>
/// Checks if field's number is valid.
/// </summary>
/// <remarks>
/// References to https://developers.google.com/protocol-buffers/docs/proto3#assigning_field_numbers
/// </remarks>
let isFieldNumberValid (MessageFieldNumber n) =
    n >= 1u
 && n <= 536870911u
 && not (n >= 19000u && n <= 19999u)
