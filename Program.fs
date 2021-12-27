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


let nameNotLessThanNCharacters n data =
    if String.length data.Name < n then
        Ok data
    else
        Error "Name cannot be less than 10 characters!"

let validateRequest1 data =
    data
    >>= nameNotBlank
    >>= nameNotLessThanNCharacters 10
    >>= emailNotBlank

let userData1 = { Name = ""; Email = "kamaki.h4@gmail.com" }

// validateRequest1 (Ok userData1)

// EX2
// Multiple parameter validators, and using different applicatives and functors
// to lift and construct different unlifted types into monadic ones.
// We'll reuse other previous validator functions.

let decoratedNamed f l data =
    let r = f + data.Name + l
    Ok r

let doIOStuff data =
    // Simulate exception->error
    // Usually, IO stuff returns unit
    if data |> String.endsWith "Sir" then
        Ok ()
    else
        Error "Name must ends with Sir!"

let validateRequest2 data =
    data
    >>= decoratedNamed "Mr " " Sir"
    >>= doIOStuff

let userData2 = { Name = "Lancelot"; Email = "lancelot@email.com" }