module Main


type mynat = | Zero | Succ : mynat -> mynat

val mynat_of_nat : nat -> mynat
let rec mynat_of_nat = function
    | 0 -> Zero
    | n -> Succ (mynat_of_nat (n - 1))

val nat_of_mynat : mynat -> nat
let rec nat_of_mynat = function
    | Zero -> 0
    | Succ n -> 1 + nat_of_mynat n

val nat_of_mynat_of_nat_is_id : n:nat -> Lemma (nat_of_mynat (mynat_of_nat n) = n)
let rec nat_of_mynat_of_nat_is_id = function
    | 0 -> ()
    | n -> nat_of_mynat_of_nat_is_id (n - 1)

val mynat_of_nat_of_mynat_is_id : n:mynat -> Lemma (mynat_of_nat (nat_of_mynat n) = n)
let rec mynat_of_nat_of_mynat_is_id = function
    | Zero -> ()
    | Succ n -> mynat_of_nat_of_mynat_is_id n


type pos = n:int{n>0}

val nat2pos : nat -> pos
let nat2pos n = n + 1

val pos2nat : pos -> nat
let pos2nat n = n - 1

val nat2pos2natIsId : n:nat -> Lemma(pos2nat (nat2pos n) = n)
let nat2pos2natIsId n = ()

val pos2nat2posIsId : p:pos -> Lemma(pos2nat (nat2pos p) = p)
let pos2nat2posIsId p = ()

val nat2int : nat -> int
let nat2int n =
    if n % 2 = 0
    then  -(n / 2)
    else (n + 1) / 2

val int2nat : int -> nat
let int2nat i =
    if i <= 0
    then begin
        assert (i + i <= 0);
        -(i + i)
    end
    else (i + i) - 1

val nat2int2natIsId : n:nat -> Lemma(int2nat (nat2int n) = n)
let nat2int2natIsId n = ()

val int2nat2intIsId : i:int -> Lemma(nat2int (int2nat i) = i)
let int2nat2intIsId i = ()
