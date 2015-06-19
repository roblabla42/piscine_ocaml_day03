(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 20:03:26 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/19 21:19:49 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst lst =
    let rec inner_l cmp min max = function
        | Nil -> true
        | Node(a, b, c) -> if a < cmp && a > min then (inner_l a min a b) && (inner_u a a max c)
                           else                       false
    and inner_u cmp min max = function
        | Nil -> true
        | Node(a, b, c) -> if a > cmp && a < max then (inner_l a min a b) && (inner_u a a max c)
                           else                       false
    in
    match lst with
        | Nil -> true
        | Node(a, b, c) -> (inner_l a min_int a b) && (inner_u a a max_int c)

let is_perfect tree =
    let rec inner = function
        | Nil -> true
        | Node(_, Node(_, _, _), Nil) | Node(_, Nil, Node(_, _, _)) -> false
        | Node(_, a, b) -> inner a && inner b
    in
    is_bst tree && inner tree


let () =
    let x = Node(5, Node(3, Node(2, Node(1, Node(5, Nil, Nil), Nil), Nil), Nil), Nil) in
    print_endline (string_of_bool (is_bst x));;
