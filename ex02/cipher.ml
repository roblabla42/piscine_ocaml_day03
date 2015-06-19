(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 18:26:10 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/19 19:28:12 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let caesar n = String.map (fun c ->
    let a_lo = int_of_char 'a' in
    let a_up = int_of_char 'A' in
    let mymod a b = let tmp = a mod b in if tmp < 0 then tmp + b else tmp in
    if 'a' <= c && c <= 'z' then char_of_int ((mymod (int_of_char c - a_lo + n) 26) + a_lo)
    else if 'A' <= c && c <= 'Z' then char_of_int ((mymod (int_of_char c - a_up + n) 26) + a_up)
    else                            c)

let rot42 = caesar 42

let xor key = String.map (fun c -> char_of_int ((int_of_char c) lxor key))

let ft_crypt str fns : string =
    let rec apply str = function
        | [] -> str
        | fn :: b -> apply (fn str) b
    in
    apply str fns

let () =
    print_endline (ft_crypt "Salut les poneys!" [rot42; (caesar 12); (xor 2) ])
