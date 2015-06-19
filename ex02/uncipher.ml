(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 19:09:26 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/19 19:28:15 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let uncaesar n = String.map (fun c ->
    let a_lo = int_of_char 'a' in
    let a_up = int_of_char 'A' in
    let mymod a b = let tmp = a mod b in if tmp < 0 then tmp + b else tmp in
    if 'a' <= c && c <= 'z' then char_of_int ((mymod (int_of_char c - a_lo - n) 26) + a_lo)
    else if 'A' <= c && c <= 'Z' then char_of_int ((mymod (int_of_char c - a_up - n) 26) + a_up)
    else                            c)

let unrot42 = uncaesar 42

let xor key = String.map (fun c -> char_of_int ((int_of_char c) lxor key))

let ft_uncrypt str fns : string =
    let rec apply str = function
        | [] -> str
        | fn :: b -> apply (fn str) b
    in
    apply str fns

let () =
    print_endline (ft_uncrypt "Walut\"lew\"psrecw#" [(xor 2); (uncaesar 12); unrot42])
