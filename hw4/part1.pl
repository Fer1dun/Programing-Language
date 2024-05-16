

:- use_module(library(heaps)).
:- use_module(library(pairs)).




place(admin_office).
place(library).
place(cafeteria).
place(social_sciences).
place(engineering_bld).
place(lecture_hall).
place(institue_y).
place(institue_x).

road(admin_office,library,1).
road(cafeteria,social_sciences,2).
road(admin_office,engineering_bld,3).
road(engineering_bld,lecture_hall,2).
road(library,social_sciences,2).
road(library,institue_y,3).
road(lecture_hall,institue_y,3).
road(admin_office,cafeteria,4).
road(cafeteria,library,5).
road(engineering_bld,library,5).
road(social_sciences,institue_x,8).


delivery_personel(personel_1).
delivery_personel(personel_2).
delivery_personel(personel_3).

capasity(personel_1,15).
capasity(personel_2,15).
capasity(personel_3,20).

run_time(personel_1,100).
run_time(personel_2,100).
run_time(personel_3,100).

work_info(personel_1,none).
work_info(personel_2,none).
work_info(personel_3,none).


personel_place(personel_1,institue_y).
personel_place(personel_2,cafeteria).
personel_place(personel_3,lecture_hall).

object(obj1).
object(obj2).
object(obj3).
object(obj4).
object(obj5).

obj_weight(obj1,2).
obj_weight(obj2,4).
obj_weight(obj3,6).
obj_weight(obj4,8).
obj_weight(obj5,10).

pick_up_place(obj1,engineering_bld).
pick_up_place(obj2,library).
pick_up_place(obj3,institue_y).
pick_up_place(obj4,admin_office).
pick_up_place(obj5,social_sciences).

drop_place(obj1,library).
drop_place(obj2,social_sciences).
drop_place(obj3,admin_office).
drop_place(obj4,institue_y).
drop_place(obj5,institue_x).

urgency(obj1,low).
urgency(obj2,high).
urgency(obj3,med).
urgency(obj4,high).
urgency(obj5,med).

deliver_obj(obj1,none).
deliver_obj(obj2,none).
deliver_obj(obj3,personel_3).
deliver_obj(obj4,personel_2).
deliver_obj(obj5,personel_1).
   

connected(X, Y, L) :- road(X, Y, L).
connected(X, Y, L) :- road(Y, X, L).

% Path finding
path(A, B, Path, Length) :-
       travel(A, B, [A], Q, Length),
       reverse(Q, Path).

travel(A, B, P, [B|P], L) :-
       connected(A, B, L).
travel(A, B, Visited, Path, L) :-
       connected(A, C, D),
       C \== B,
       \+member(C, Visited),
       travel(C, B, [C|Visited], Path, L1),
       L is D+L1.

% Find the shortest path
shortest_path(A, B, Path, Length) :-
   setof([P,L], path(A, B, P, L), Set),
   Set = [|], % fail if no path exists
   minimal(Set, [Path, Length]).

minimal([F|R], M) :- min(R, F, M).

% Determines the minimum path
min([], M, M).
min([[P,L]|R], [_,M], Min) :-
   L < M, !, min(R, [P,L], Min).
min([_|R], M, Min) :-
   min(R, M, Min).


travel_time(CurrentLocation, Destination, Time) :-
    road(CurrentLocation, Destination, Time).
travel_time(CurrentLocation, Destination, Time) :-
    road(Destination, CurrentLocation, Time).

calculate_total_time(DeliveryPerson, Object, TotalTime) :-
    personel_place(DeliveryPerson, CurrentLocation),
    pick_up_place(Object, PickupPlace),
    drop_place(Object, DropoffPlace),
    travel_time(CurrentLocation, PickupPlace, PickupTime),
    travel_time(PickupPlace, DropoffPlace, DeliveryTime),
    TotalTime is PickupTime + DeliveryTime.
  


is_delivery_person_available(Object, DeliveryPerson) :-
    delivery_personel(DeliveryPerson),
    capasity(DeliveryPerson, Capacity),
    obj_weight(Object, Weight),
    Capacity >= Weight,
    pick_up_place(Object,pick),
    personel_place(DeliveryPerson,place),
    pick = place.


is_object_in_transit(Object, DeliveryPerson, TotalTime) :-
    deliver_obj(Object, DeliveryPerson).
    

