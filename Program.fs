module Program

open System.Text.RegularExpressions

type IPv4 =
  {
    Octet1 : int
    Octet2 : int
    Octet3 : int
    Octet4 : int
  }






type Location =
  | Localhost
  | Internal
  | Public







let location ip =
  match ip with
    | { Octet1 = 10 }                                          -> Internal
    | { Octet1 = 192; Octet2 = 168 }                           -> Internal
    | { Octet1 = 172 } when ip.Octet2 >= 16 && ip.Octet2 <= 32 -> Internal
    | { Octet1 = 127; Octet2 = 0; Octet3 = 0; Octet4 = 1 }     -> Localhost
    | _ -> Public






let localHost = { Octet1 = 127; Octet2 = 0; Octet3 = 0; Octet4 = 1 }
let googleDNS = { Octet1 = 8; Octet2 = 8; Octet3 = 8; Octet4 = 8 }
let myIp = { Octet1 = 192; Octet2 = 168; Octet3 = 10; Octet4 = 56 }

let lhLocation = location localHost
let googleDNSLocation = location googleDNS
let myIpLocation = location myIp












let location2 ip =
  match ip with
    | (127,0,0,1) -> Localhost
    | (10,_,_,_)
    | (192,168,_,_) -> Internal
    | (172,x,_,_) when x >=16 && x <= 32 -> Internal
    | _ -> Public







let localHost2 = (127,0,0,1)
let googleDNS2 = (8,8,8,8)
let myIp2 = (192,168,56,10)

let lhLocation2 = location2 localHost2
let googleDNSLocation2 = location2 googleDNS2
let myIpLocation2 = location2 myIp2








let directTraffic ip =
  match location ip with
    | Localhost ->
      //do something
      ()
    | Internal ->
      //something else
      ()
    | Public ->
      failwith "how did a public IP get here?"




type Phone = { Area : string; Prefix : string; Number : string }
type Email = { Mailbox : string; Domain : string }

type SpecialString =
  | Phone of Phone
  | Email of Email
  | IPv4 of IPv4
  | Normal of string







let (|Email|_|) input =
   let m = Regex.Match(input,@".+@.+")
   if (m.Success) then Some input else None

let (|IPv4|_|) input =
   let m = Regex.Match(input,"""^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$""")
   if (m.Success) then Some input else None

let (|Phone|_|) input =
   let m = Regex.Match(input,"""^\(?([0-9]{3})\)?[-.●]?([0-9]{3})[-.●]?([0-9]{4})$""")
   if (m.Success) then Some input else None





let parseString (str : string) =
    match str with
      | Email x ->
        let parts = x.Split('@')
        Email { Mailbox = parts.[0]; Domain = parts.[1] }
      | IPv4 x ->
        let parts = x.Split('.') |> Array.map (fun part -> System.Convert.ToInt32(part))
        IPv4 { Octet1 = parts.[0]; Octet2 = parts.[1]; Octet3 = parts.[2]; Octet4 = parts.[3] }
      | Phone x ->
        let parts = x.Split('-')
        Phone { Area = parts.[0]; Prefix = parts.[1]; Number = parts.[2] }
      | x -> Normal x



let email = parseString "test@gmail.com"
let phone = parseString "214-681-1111"
let ip = parseString "127.0.0.1"
let other = parseString "The quick brown fox jumps over the lazy dog"

//System.Console.ReadKey() |> ignore
