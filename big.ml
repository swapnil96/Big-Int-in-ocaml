
(* Defining the type *)
type bigint = sign*int list
and sign = Neg|NonNeg;;

exception Divisor_is_0

(* Fuction for printing list*)
let rec print (l) = match l with
	|[]		-> 	print_char('\n')
	|a::b 	-> 	print_int a; print (b)
;;	


(* Function to remove unnecessary 0s *)
let rec process (num1) = match num1 with
	|[] 	-> 	num1
	|a::b 	-> 	if a = 0 then
					if b = [] then
						num1
					else		
						process (b)

				else
					num1
;;


(* Fuction for length *)
let rec length (l) = match l with
	|[] 	->	0
	|a::b 	-> 	1 + length (b)
;;


(* Function to reverse list *)
let rec reverse (accumulate) (l) = match l with
	|[] 	->	accumulate
	|a::b -> 	reverse (a::accumulate) (b)
;;


(* Fuction for helping in inserting at start *)
let rec insert_help (l) (n) = match n with
	|0 -> 	l 
	|_ -> 	0 :: insert_help (l) (n-1)
;;		


(* Main function for inserting at start *)
let insert (num1) (num2) = 

	let l1 = length (num1) in
	let l2 = length (num2) in

	if (l1 > l2) then
		let num2 = insert_help (num2) (l1-l2) in
		(num1,num2);

	else if (l2 > l1) then
		let num1 = insert_help (num1) (l2-l1) in
		(num1,num2);
	
	else
		(num1,num2);	
;;	


(* Fuctions for spliting the tuples *)
let split_list_num1 (x,y) = x
;;

let split_list_num2 (x,y) = y
;;


(* Fuction to compare 2 list of digits *)
let compare_list (num1) (num2) = 

	let n1 = split_list_num1 (insert (num1) (num2)) in
	let n2 = split_list_num2 (insert (num1) (num2)) in

	let rec help x1 x2 = match x1, x2 with
		| [], []         -> 0
		| [], x | x, []  -> 0
		| a1::b1, a2::b2 -> if a1 > a2 then 1
							else if a1 < a2 then -1
							else help b1 b2 in	

	let o = help n1 n2 in
	o;
;;	


(* Function to split bigints *)
let split_bigint_sign (x, y) = x
;;

let split_bigint_list (x, y) = y
;;


(* Function to print bigint *)
let print_bigint (x,y) = match x with
	|Neg 	->	print_string("Neg, "); print(y)
	|NonNeg	->  print_string("NonNeg, "); print(y)
;;


(* Main fuction to compare two bigints *)
let compare (num1) (num2) = 

	let s1 = split_bigint_sign (num1) in
	let l1 = split_bigint_list (num1) in

	let s2 = split_bigint_sign (num2) in
	let l2 = split_bigint_list (num2) in
	
	if s1 = NonNeg && s2 = Neg then
		let fin = 1 in
		fin

	else if s1 = Neg && s2 = NonNeg then
		let fin = -1 in
		fin

	else if s1 = NonNeg && s2 = NonNeg then
		let fin = compare_list (l1) (l2) in
		fin

	else
		let fin = compare_list (l1) (l2) in
		let fin = fin * (-1) in
		fin			 
;;


(* Main function to subtract 2 list *)
let subtract_list (num1) (num2) = 

	let l1 = split_list_num1 (insert (num1) (num2)) in
	let l2 = split_list_num2 (insert (num1) (num2)) in

	let l1 = reverse [] (l1) in
	let l2 = reverse [] (l2) in

	let rec help (a1) (a2) (c) = match a1, a2 with
		|[], []         	->	if c = 0 then [] else [c]
		|[], x | x, []   	->  help [0] x c
		|ax::bx, cx::dx		->  if (ax + c) >= cx then 
									let m = ax - cx + c in 		 
							 		m::help (bx) (dx) (0);	
								else	
							 		let m = ax + 10 - cx + c in
						   			m:: help (bx) (dx) (-1); in			

	let fin = help (l1) (l2) 0 in
	let ans = reverse [] (fin) in
	ans;
						   		
;;



(* Main function to add 2 list *)
let sum_list (num1) (num2) = 
		
	let l1 = split_list_num1 (insert (num1) (num2)) in
	let l2 = split_list_num2 (insert (num1) (num2)) in

	let l1 = reverse [] (l1) in
	let l2 = reverse [] (l2) in	
	
	let rec help (a1) (a2) (c) = match a1, a2 with
		|[], []        ->  if c = 0 then [] else [c]
		|[], x | x, [] ->  help [0] x c
		|ax::bx, cx::dx    ->  let s = ax + cx + c in
						   (s mod 10):: help bx dx (s / 10) in			

	let fin = help (l1) (l2) (0) in
	let ans = reverse [] (fin) in
	ans;
;;


(* Main function to subtract 2 bigints *)
let subtract (num1) (num2) = 

	let s1 = split_bigint_sign (num1) in
	let l1 = split_bigint_list (num1) in

	let s2 = split_bigint_sign (num2) in
	let l2 = split_bigint_list (num2) in
	
	if s1 = NonNeg && s2 = NonNeg then
		let com = compare_list (l1) (l2) in
		if com = 1 then
			let fin = subtract_list (l1) (l2) in
			let fin = process (fin) in 
			let ans = (NonNeg, fin) in
			ans;

		else if com = -1 then	
			let fin = subtract_list (l2) (l1) in
			let fin = process (fin) in 
			let ans = (Neg, fin) in
			ans;

		else
			let ans = (NonNeg, [0]) in
			ans;	

	else if s1 = NonNeg && s2 = Neg then
		let fin = sum_list (l1) (l2) in
		let fin = process (fin) in 
		let ans = (NonNeg, fin) in
		ans;

	else if s1 = Neg && s2 = NonNeg then
		let fin = sum_list (l1) (l2) in
		let fin = process (fin) in 
		let ans = (Neg, fin) in
		ans;

	else
		let com = compare_list (l1) (l2) in
		if com = 1 then
			let fin = subtract_list (l1) (l2) in
			let fin = process (fin) in 
			let ans = (Neg, fin) in
			ans;

		else if com = -1 then	
			let fin = subtract_list (l2) (l1) in
			let fin = process (fin) in 
			let ans = (NonNeg, fin) in
			ans;

		else
			let ans = (NonNeg, [0]) in 
			ans;	
;;


(* Function for addition of 2 bigints *)
let sum (num1) (num2) = 

	let s1 = split_bigint_sign (num1) in
	let l1 = split_bigint_list (num1) in

	let s2 = split_bigint_sign (num2) in
	let l2 = split_bigint_list (num2) in
	
	if s1 = NonNeg && s2 = NonNeg then
		let fin = sum_list (l1) (l2) in
		let fin = process (fin) in 
		let ans = (NonNeg, fin) in
		ans;

	else if s1 = NonNeg && s2 = Neg then
		let com = compare_list (l1) (l2) in
		if com = 1 then
			let fin = subtract_list (l1) (l2) in
			let fin = process (fin) in 
			let ans = (NonNeg, fin) in
			ans;

		else if com = -1 then	
			let fin = subtract_list (l2) (l1) in
			let fin = process (fin) in 
			let ans = (Neg, fin) in
			ans;

		else
			let ans = (NonNeg, [0]) in
			ans;	

	else if s1 = Neg && s2 = NonNeg then
		let com = compare_list (l1) (l2) in
		if com = 1 then
			let fin = subtract_list (l1) (l2) in
			let fin = process (fin) in 
			let ans = (Neg, fin) in
			ans;

		else if com = -1 then
			let fin = subtract_list (l2) (l1) in
			let fin = process (fin) in 
			let ans = (NonNeg, fin) in
			ans;

		else
			let ans = (NonNeg, [0]) in
			ans;	

	else
		let fin = sum_list (l1) (l2) in
		let fin = process (fin) in 
		let ans = (Neg, fin) in
		ans;		
;;		


(* Function to provide offset to sum *)
let rec offset (num1) (len) = match len with
	|0	->	num1
	|_ 	-> 	let temp = num1 @ [0] in 
			offset (temp) (len-1)
;;		


(* Helper funtion to multiply list and int *)
let rec multiply_int_help (l) (n) (c) = match l with
	|[] 	->  if c = 0 then []
				else [c]
	|a::b	->	let s = a * n + c in
			 	if s > 9 then (s mod 10) :: multiply_int_help (b) (n) (s/10)
			 	else s :: multiply_int_help (b) (n) (0)
;;


(* Funtion to multiply list and int *)
let multiply_int (num1) (num2) = 
	
	let temp = multiply_int_help (num1) (num2) (0) in
	let fin = reverse [] temp in
	fin;
;;		


(* Main function to multiply list *)
let multiply_list (num1) (num2) = 

	let rec func (n1) (n2) (fin) (off) = match n2 with 
		|[]	 	-> 	fin
		|a::b 	->	let t = multiply_int (n1) (a) in
					let t = offset (t) (off) in
					let temp = sum_list (t) (fin) in
					func (n1) (b) (temp) (off+1)
					
	in

	let num1 = reverse [] num1 in  
	let num2 = reverse [] num2 in  
	let ans = func (num1) (num2) ([0]) (0) in
	ans;
;;


(* Main function to multiply bigints *)
let multiply (num1) (num2) = 

	let s1 = split_bigint_sign (num1) in
	let l1 = split_bigint_list (num1) in

	let s2 = split_bigint_sign (num2) in
	let l2 = split_bigint_list (num2) in
	
	if s1 = NonNeg && s2 = NonNeg then
		let fin = multiply_list (l1) (l2) in
		let fin = process (fin) in 
		let ans = (NonNeg, fin) in
		ans;

	else if s1 = Neg && s2 = Neg then
		let fin = multiply_list (l1) (l2) in
		let fin = process (fin) in 
		let ans = (NonNeg, fin) in
		ans;

	else
		let fin = multiply_list (l1) (l2) in
		let fin = process (fin) in 
		let comp = compare_list (fin) ([0]) in
		if comp = 0 then 
			let ans = (NonNeg, [0]) in
			ans;

		else
			let ans = (Neg, fin) in
			ans;		
;;		


(* Funtion that gives quotient *)
let quotient_help (num1) (num2) =

	let rec help (num11) (num21) (temp) = 

		let now1 = multiply_list (num21) (temp) in
		let temp1 = sum_list (temp) ([1]) in
		let now2 = multiply_list (num21) (temp1) in
		let comp1 = compare_list (num11) (now1) in
		let comp2 = compare_list (num11) (now2) in

		if comp1 = 0 then 
			temp

		else	
			if comp1 = 1 && comp2 = 1 then
				help (num1) (num2) (temp1)

			else if comp1 = 1 && comp2 = -1 then
				temp

			else if comp1 = 1 && comp2 = 0 then
				temp1

			else
				temp	

	in

	let fin = help (num1) (num2) ([1]) in
	fin;
;;	


(* Funtion that gives remainder *)
let remainder_help (num1) (num2) (num3) = 

	let temp = multiply_list (num2) (num3) in
	let fin = subtract_list (num1) (temp) in
	fin;
;;


(* Funtion to build quotient simultaneously *)
let append (num1) (num2) = 

	let rec help (n1) (n2) = match n2 with
	|[] 	-> 	n1
	|a::b   -> 	let n1 = n1 @ [a] in 
				help (n1) (b) 

	in

	let l = help (num1) (num2) in
	l;
;;	 


(* Main function to divide 2 list *)
let division_list (num1) (num2) = 
	
	let rec help (dividend) (divisor) (quotient) (temp) (remainder)= match dividend with
	|[] 	->	quotient
	|a::b 	->	let temp = temp @ [a] in 
				let comp = compare_list temp divisor in
				if comp = -1 then
					let quotient = quotient @ [0] in
					let remainder = temp in
					help (b) (divisor) (quotient) (temp) (remainder)

				else
					let quo = quotient_help (temp) (divisor) in
					let rem = remainder_help (temp) (divisor) (quo) in
					let quotient = append (quotient) (quo) in
					help (b) (divisor) (quotient) (rem) (rem) in

	let t = help (num1) (num2) ([]) ([]) ([]) in
	t;  
;;		


(* Main function to find quotient of 2 bigints *)
let quotient (num1) (num2) = 

	let s1 = split_bigint_sign (num1) in
	let l1 = split_bigint_list (num1) in

	let s2 = split_bigint_sign (num2) in
	let l2 = split_bigint_list (num2) in
	
	let comp = compare_list (l2) ([0]) in
		
	try 
		if comp = 0 then 
			raise (Divisor_is_0);	
	
		let comp = compare_list (l1) ([0]) in
		if comp = 0 then
			let ans = (NonNeg, [0]) in
			ans;

		else	
			if s1 = NonNeg && s2 = NonNeg then
				let fin = division_list (l1) (l2) in
				let fin = process (fin) in 
				let ans = (NonNeg, fin) in
				ans;

			else if s1 = Neg && s2 = Neg then
				let fin = division_list (l1) (l2) in
				let fin = process (fin) in 
				let ans = (NonNeg, fin) in
				ans;

			else
				let fin = division_list (l1) (l2) in
				let rem = subtract_list (l1) (multiply_list (fin) (l2)) in 
				let comp = compare_list (rem) ([0]) in 
				let fin = process (fin) in 
				if comp != 0 then
					let fin = sum_list (fin) ([1]) in   
						let ans = (Neg, fin) in
							ans;
	
				else
					let ans = (Neg, fin) in
					ans;

	with
		Divisor_is_0 -> print_string("Error - Division by 0\n");
							let ans = (NonNeg, []) in
							ans;				
;;		



(* Main function to find remainder 2 bigints *)
let remainder (num1) (num2) = 

	let comp = compare (num2) (NonNeg, [0]) in
	if  comp = 0 then 
		raise (Division_by_zero);	

	let quo = quotient (num1) (num2) in
	let fin = subtract (num1) (multiply (quo) (num2)) in 
	fin;		
;;	


(* Function for absolute value *)
let absolute (num1) = 

	let l1 = split_bigint_list (num1) in
	let ans = (NonNeg, l1) in
	ans;
;;


(* Function for absolute value *)
let negation (num1) = 

	let s1 = split_bigint_sign(num1) in
	let l1 = split_bigint_list(num1) in
	if s1 = Neg then
		let ans = (NonNeg, l1) in	
		ans 
	else
		let ans = (Neg, l1) in	
		ans 
;;


(* Function for equality check *)
let equal (num1) (num2) = 

	let t = compare (num1) (num2) in
	if t = 0 then
		true

	else
		false	
;;


(* Function for greater_than check *)
let greater_than (num1) (num2) = 

	let t = compare (num1) (num2) in
	if t = 1 then
		true

	else
		false	
;;


(* Function for great_or_equal check *)
let great_or_equal (num1) (num2) = 

	let t = compare (num1) (num2) in
	if t = 1 || t = 0 then
		true

	else
		false	
;;


(* Function for less_than check *)
let less_than (num1) (num2) = 

	let t = compare (num1) (num2) in
	if t = -1 then
		true

	else
		false	
;;


(* Function for less_or_equal check *)
let less_or_equal (num1) (num2) = 

	let t = compare (num1) (num2) in
	if t = -1 || t = 0 then
		true

	else
		false	
;;


(* Function to convert int to bigint *)
let convert (num1) = 

	let rec help (n1) (accumulate) = match n1 with 
		|0	->	accumulate
		|_ 	-> 	let r = n1 mod 10 in
				let q = n1 / 10 in
				let accumulate = r :: accumulate in
				help (q) (accumulate) in

	if num1 < 0 then 
		let t = help (-1*num1) ([]) in
		let ans = (Neg, t) in
		ans;

	else if num1 > 0 then
		let t = help (num1) ([]) in
		let ans = (NonNeg, t) in
		ans;

	else
		let ans = (NonNeg, [0]) in
		ans;	
;;


let to_string (num1) = 

	let s1 = split_bigint_sign(num1) in
	let l1 = split_bigint_list(num1) in

	let rec help (n1) = match n1 with
		|[] 	->	""
		|a::b 	-> 	(string_of_int a ) ^ help (b)  in

	let t = help (l1) in
	if s1 = NonNeg then
		t

	else
		let ans = "-" ^ t in
		ans;				
;;	


let n = Neg;;	
let nn = NonNeg;;
let a = [1;2;3;4;5;6;7;8;9];;
let b = [9;8;7;6;5;4;3;2;1];;
let l1 = (n, a);;
let l2 = (n, b);;
let l3 = (nn, a);;
let l4 = (nn, b);;

let s1 = subtract (l4) (l1);;
print_string("Subtract -> Neg, 123456789 from NonNeg, 987654321 \n\t");;
print_bigint(s1);;

let s2 = subtract (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l1);;
print_string("Subtract -> Neg, 123456789 from Neg, 1453245632134 \n\t");;
print_bigint(s2);;

let s3 = subtract (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Subtract -> NonNeg, 123456789 from Neg, 1453245632134 \n\t");;
print_bigint(s3);;

let s4 = subtract (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Subtract -> NonNeg, 123456789 from NonNeg, 1453245632134 \n\t");;
print_bigint(s4);;

print_char('\n');;

let a1 = sum (l2) (l3);;
print_string("Addition -> Neg, 987654321 to NonNeg, 123456789\n\t");;
print_bigint(a1);;

let a2 = sum (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l1);;
print_string("Addition -> Neg, 123456789 to Neg, 1453245632134 \n\t");;
print_bigint(a2);;

let a3 = sum (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Addition -> NonNeg, 123456789 to Neg, 1453245632134 \n\t");;
print_bigint(a3);;

let a4 = sum (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Addition -> NonNeg, 123456789 to NonNeg, 1453245632134 \n\t");;
print_bigint(a4);;

print_char('\n');;

let m1 = multiply (l2) (l3);;
print_string("Multiplication -> Neg, 987654321 to NonNeg, 123456789 \n\t");;
print_bigint(m1);;

let m2 = multiply (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l1);;
print_string("Multiplication -> Neg, 123456789 to Neg, 1453245632134 \n\t");;
print_bigint(m2);;

let m3 = multiply (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Multiplication -> NonNeg, 123456789 to Neg, 1453245632134 \n\t");;
print_bigint(m3);;

let m4 = multiply (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Multiplication -> NonNeg, 123456789 to NonNeg, 1453245632134 \n\t");;
print_bigint(m4);;

print_char('\n');;


let d1 = quotient (l2) (l1);;
print_string("Quotient -> Neg, 987654321 divide by Neg, 123456789 \n\t");;
print_bigint(d1);;

let d2 = quotient (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Quotient -> Neg, 1453245632134 divide by NonNeg, 123456789\n\t");;
print_bigint(d2);;

let d3 = quotient (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Quotient -> NonNeg, 1453245632134 divide by NonNeg, 123456789 \n\t");;
print_bigint(d3);;

let d4 = quotient (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l1);;
print_string("Quotient -> NonNeg, 1453245632134 divide by Neg, 123456789 \n\t");;
print_bigint(d4);;

print_char('\n');;

let r1 = remainder (l2) (l3);;
print_string("Remainder -> Neg, 987654321 divide by NonNeg, 123456789 \n\t");;
print_bigint(r1);;

let r2 = remainder (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l3);;
print_string("Remainder -> NonNeg, 1453245632134 divide by NonNeg, 123456789\n\t");;
print_bigint(r2);;

let r3 = remainder (NonNeg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l1);;
print_string("Remainder -> NonNeg, 1453245632134 divide by Neg, 123456789 \n\t");;
print_bigint(r3);;

let r4 = remainder (Neg, [1;4;5;3;2;4;5;6;3;2;1;3;4]) (l1);;
print_string("Remainder -> NonNeg, 1453245632134 divide by Neg, 123456789 \n\t");;
print_bigint(r4);;

print_char('\n');;

let e1 = equal (l1) (l1);;
print_string("Equality -> -123456789 to -123456789 \n\t");;
if e1 = true then
	print_string("True\n")
else 
	print_string("False\n");;

print_char('\n');;

let g1 = greater_than (l1) (l1);;
print_string("Greater than -> -123456789 to -123456789 \n\t");;
if g1 = true then
	print_string("True\n")
else 
	print_string("False\n");;

print_char('\n');;

let ge1 = great_or_equal (l1) (l1);;
print_string("Greater than equal to-> -123456789 to -123456789 \n\t");;
if ge1 = true then
	print_string("True\n")
else 
	print_string("False\n");;

print_char('\n');;

let les1 = less_than (l1) (l1);;
print_string("Lesser than -> -123456789 to -123456789 \n\t");;
if les1 = true then
	print_string("True\n")
else 
	print_string("False\n");;

print_char('\n');;

let lese1 = less_or_equal (l1) (l1);;
print_string("Less than equal to-> -123456789 to -123456789 \n\t");;
if lese1 = true then
	print_string("True\n")
else 
	print_string("False\n");;

print_char('\n');;

print_string("Converting -> -123456789 to bigint\n\t");;
let y = convert(-123456789);;
print_bigint(y);;

print_string("Converting -> 0 to bigint\n\t");;
let y = convert(0);;
print_bigint(y);;

print_char('\n');;

print_string("Negating -> 123456789 to -123456789\n\t");;
let n1 = negation(l3);;
print_bigint(n1);;

print_char('\n');;

print_string("Absoulte value -> -123456789 to 123456789\n\t");;
let ab1 = absolute(l1);;
print_bigint(n1);;

print_char('\n');;

print_string("Converting bigint to string -> Neg, 123456789 to -123456789\n\t");;
let st1 = to_string(l1);;
print_string(st1);;

print_char('\n');;
print_char('\n');;

let m1 = multiply (n, [1;2;3]) (nn, [0]);;
print_string("Multiplication -> Neg, 123 to NonNeg, 0 \n\t");;
print_bigint(m1);;

print_char('\n');;

print_string("Error in trying to divide -> NonNeg, 124 by NonNeg, 0\n\t");;
let t = quotient ((Neg, [1;2;4])) ((NonNeg, [0]));;
print_char('\t');;
print_bigint(t);;

print_char('\n');;

let d1 = quotient ((NonNeg, [0])) (l1);;
print_string("Quotient -> 0 divide by -123456789 \n\t");;
print_bigint(d1);;

print_char('\n');;

let r1 = remainder ((NonNeg, [0])) (l1);;
print_string("Remainder -> 0 divide by -123456789 \n\t");;
print_bigint(r1);;


let createBigint a = 
    convert a
;;

let bigadd a b = 
	sum a b
;;	

let bigsub a b = 
	subtract a b
;;	

let bigmult a b = 
	multiply a b
;;	

let bigquot a b = 
	quotient a b
;;	

let bigrem a b = 
	remainder a b
;;	

let bigabs a = 
	absolute a
;;	

let bigneg a = 
	negation a
;;	

let bigequal a b = 
	equal a b
;;	

let bigless a b = 
	less_than a b
;;	

let biggreater a b = 
	greater_than a b
;;	

let biglesseq a b = 
	less_or_equal a b
;;	

let biggreatereq a b = 
	great_or_equal a b
;;	

let bigstr a = 
	to_string a
;;	
