module Enumerations

type EnumerationName = EnumerationName of string

type EnumerationValue = EnumerationValue of int
    
type EnumerationValueInterval = {
    min: EnumerationValue
    max: EnumerationValue
}

type EnumerationReserveItem =
    | Value of EnumerationValue
    | Interval of EnumerationValueInterval
    
type EnumerationReserve =
    | EnumerationValues of EnumerationReserveItem seq
    | EnumerationNames of EnumerationName seq

type EnumerationCase = {
    name: string
    value: EnumerationValue
}

type EnumerationItem =
    | EnumerationCase of EnumerationCase
    | EnumerationReserve of EnumerationReserve

type Enumeration = {
    name: string
    items: EnumerationItem seq
}
