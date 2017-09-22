let rec gcd (m, n) =
  if m = 0
  then n
  else gcd (n mod m, m)

let rec power (x, k) =
  if k = 1
  then x
  else if k mod 2 = 0
  then power (x *. x, k / 2)
  else x *. power (x *. x, k / 2)

let nextfib (prev, curr) = curr, prev + curr

let rec fibpair n =
  if n = 1 then (0, 1) else nextfib (fibpair (n - 1))

let fib n =
  let rec itfib n prev curr =
    if n = 1
    then curr
    else itfib (n - 1) curr (prev + curr)
  in itfib n 0 1

let rec introot n =
  let increase k n = if (k + 1) * (k + 1) > n then k else k + 1 in
  if n = 0
  then 0
  else increase (2 * introot (n / 4)) n

let rec gcd_ex (m, n) =
  if m = n || m = 0
  then n
  else if m mod 2 = 0
       then if n mod 2 = 0
            then 2 * gcd_ex (m / 2, n / 2)
            else gcd_ex (m / 2, n)
       else if n mod 2 = 0
            then gcd_ex (n mod m, m)
            else if m < n then gcd_ex ((n / 2) - (m / 2), m)
                          else gcd_ex (n mod m, m)

let sqroot a =
  let acc = 1.0E-10 in
  let rec findroot x =
    let nextx = (a /. x +. x) /. 2.0 in
    if abs_float (x -. nextx) < acc *. x
    then nextx
    else findroot nextx
  in findroot 1.0

let rec pos d = 1.0 /. d +. neg (d -. 2.0)
and neg d = if d > 0.0
            then (-1.0 /. d) +. pos (d -. 2.0)
            else 0.0
          
let () =
  Printf.printf "gcd 1245 123: %d\n" (gcd (1245, 123));
  Printf.printf "7.32 ^ 10: %f\n" (power (7.32, 10));
  Printf.printf "rec fib 30: %d\n" (snd @@ fibpair 30);
  Printf.printf "it fib 30: %d\n" (fib 30);
  Printf.printf "root 123456789: %d\n" (introot 123456789);
  Printf.printf "gcd_ex 1245 123: %d\n" (gcd_ex (1245, 123));
  Printf.printf "sqroot 1245.23: %f\n" (sqroot 1245.23);
  Printf.printf "pi: %f\n" (4.0 *. neg 8003.)
