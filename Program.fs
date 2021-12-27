open System
open FSharpPlus


// Some exercises to learn ROP/Kleisli composition subset
// Derived from Schott Wlaschin ROP examples

let inline (|>!) x f = tap f x



// EX1
// Simple, straightforward downstream function bypassing via standard Error monad
// and its Result bind operator.
type Data = { 
    Name: string
    Email: string 
}

let nameNotBlank data =
    if String.IsNullOrEmpty data.Name |> not then
        Ok data
    else
        Error "Name is blank!"

let emailNotBlank data =
    if String.IsNullOrEmpty data.Email |> not then
        Ok data
    else
        Error "Email is blank!"

let nameNotLessThanN name n =
    if String.length <| name < n |> not then
        true
    else
        false


let nameNotLessThan10Characters (data) =
    if nameNotLessThanN data.Name 10 then
        Ok data
    else
        Error "Name cannot be less than 10 characters!"

let validateRequest1 data =
    data
    >>= nameNotBlank
    >>= nameNotLessThan10Characters
    >>= emailNotBlank

let userData1 = { Name = ""; Email = "kamaki.h4@gmail.com" }

// validateRequest1 (Ok userData1)

// EX2
// Multiple parameter validators, and using different applicatives and functors
// to lift and construct different unlifted types into monadic ones.
// We'll reuse other previous validator functions.

