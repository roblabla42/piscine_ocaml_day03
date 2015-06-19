(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 13:45:55 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/19 18:25:08 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size = function
    | Nil -> 0
    | Node(_, l, r) -> 1 + (size l) + (size r)

let rec height = function
    | Nil -> 0
    | Node(_, l, r) ->
        let max i j = if i < j then j else i in
        max (height l + 1) (height r + 1)

let draw_tree tree =
    let rec ft_power x y =
        if      y = 0 then              1
        else if y = 1 then              x
        else if y mod 2 = 0 then        ft_power (x * x) (y / 2)
        else                            ft_power x (y - 1) * 2
    in
    let size = 50 in
    let first_step = (ft_power 2 (height tree) / 4) * (size) in
    let draw_square x y size =
        let mean = size / 2 in
        Graphics.moveto (x - mean) (y - mean);
        Graphics.lineto (x + mean) (y - mean);
        Graphics.lineto (x + mean) (y + mean);
        Graphics.lineto (x - mean) (y + mean);
        Graphics.lineto (x - mean) (y - mean)
    in
    let rec inner x y y_step = function
        | Node(v, left, right) -> begin
            draw_square x y size; Graphics.moveto (x - (size / 3)) y; Graphics.draw_string v;
            Graphics.moveto (x + size / 2) y; Graphics.lineto (x + 100 - (size / 2)) (y + y_step);
            inner (x + 100) (y + y_step) (y_step / 2) left;
            Graphics.moveto (x + size / 2) y; Graphics.lineto (x + 100 - (size / 2)) (y - y_step);
            inner (x + 100) (y - y_step) (y_step / 2) right;
        end
        | Nil -> (draw_square x y size; Graphics.moveto (x - (size / 3)) y; Graphics.draw_string "Nil";)
    in
    inner 50 500 first_step tree;;

let () =
    Graphics.open_graph " 2560x1440";
    draw_tree (Node ("Root", Node("SubRoot 1", Node ("SubRoot2", Nil, Nil), Node("SubRoot3", Node("Test", Node("Test2", Nil, Nil), Nil), Nil)), Nil));
    ignore (Graphics.read_key ());
