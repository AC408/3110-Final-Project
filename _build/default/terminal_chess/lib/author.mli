(** CS 3110 Spring 2022 Final Project

    @author Allen Chen (ac2324), Pujita Sunder (ps779), Michele Joseph (maj92), Anna Gray (apg74) *)

(************************************************************

   Academic Integrity Statement

   We, the people named in the author comment above, have fully reviewed the
   course academic integrity policies.  We have adhered to those policies in
   solving the assignment.

   The policies do permit some limited collaboration among students currently
   enrolled in the course. If we did engage in such collaboration, here is the
   list of other students with whom we collaborated, and a brief summary of that
   collaboration:

   - none

 ************************************************************)

val hours_worked : int
(** [hours_worked] is the number of hours you worked on this assignment. *)


(*task?: 1) make sure pieces cant take pieces of the same color, 2) make sure that piece cant jump other pieces (unless you’re a knight), 3) after parsing command, check to see if king will be in check after moving. if so, throw err and revert to prev position, 4) for checkmate, are we checking all possible paths and seeing if the king can escape (in which case, we should have a list of available moves) or just have the players declare they’re checkmated when they see they have no legal moves?, 5) stalemate -> same as 4*)