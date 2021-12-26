open System


// Some exercises to learn ROP/Kleisli composition subset
// Derived from Schott Wlaschin ROP examples



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

let (>>=) f g = Result.bind g f

let validateRequest request =
    request
    >>= nameNotBlank
    >>= nameNotLessThan10Characters
    >>= emailNotBlank

let userData = { Name = "hello world"; Email = "kamaki.h4@gmail.com" }

// validateRequest request 

// EX2
// TODO

