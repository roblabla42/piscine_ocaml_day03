(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 10:03:43 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/19 13:45:34 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
    let mean = size / 2 in
    Graphics.moveto (x - mean) (y - mean);
    Graphics.lineto (x + mean) (y - mean);
    Graphics.lineto (x + mean) (y + mean);
    Graphics.lineto (x - mean) (y + mean);
    Graphics.lineto (x - mean) (y - mean)

let draw_tree_node tree =
    let size = 50 in
    let rec inner x y = function
        | Node(v, left, right) -> begin
            draw_square x y size; Graphics.moveto (x - (size / 3)) y; Graphics.draw_string v;
            Graphics.moveto (x + size / 2) y; Graphics.lineto (x + 100 - (size / 2)) (y + 50);
            inner (x + 100) (y + 50) left;
            Graphics.moveto (x + size / 2) y; Graphics.lineto (x + 100 - (size / 2)) (y - 50);
            inner (x + 100) (y - 50) right;
        end
        | Nil -> (draw_square x y size; Graphics.moveto (x - (size / 3)) y; Graphics.draw_string "Nil";)
    in
    inner 300 300 tree;;

let () =
    Graphics.open_graph "";
    draw_tree_node (Node ("test", Nil, Nil));
    ignore (Graphics.read_key ());
