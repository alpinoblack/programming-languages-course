(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*#1.a*)
fun all_except_option(str: string, strings: string list) =
    let fun all_except_option_aux(str: string, strings: string list, newStrings: string list) =
        case strings of
            [] => NONE
            | head :: tail => if (same_string(str, head)) 
                then SOME (newStrings @ tail)
                else all_except_option_aux(str, tail, head :: newStrings)
    in
        all_except_option_aux(str, strings, [])
    end 

(* 
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
*)

(*#1.b*)
fun get_substitutions1(substitutions: string list list, str: string) = 
    case substitutions of 
        [] => []
        | head :: tail =>
            case all_except_option(str, head) of
                NONE => get_substitutions1(tail, str)
                | SOME listWithoutString => listWithoutString @ get_substitutions1(tail, str)

(*#1.b*)
fun get_substitutions2(substitutions: string list list, str: string ) =
    let fun get_substitutions2_aux(substitutions: string list list, str: string, agg: string list) =
        case substitutions of
            [] => agg
            | head :: tail => 
                case all_except_option(str, head) of 
                    NONE => get_substitutions2_aux(tail, str, agg)
                    | SOME listWithoutString => get_substitutions2_aux(tail, str, agg @ listWithoutString)
    in
        rev (get_substitutions2_aux(substitutions, str, []))
    end

(*#1.d*)
fun similar_names(substitutions: string list list, fullName: {first: string, middle: string, last: string}) =
    let
        fun create_alternative_full_names(firstNames: string list, last: string, middle: string) =
            case firstNames of
                [] => []
                | alternativeFirstName :: tail => {first = alternativeFirstName, last = last, middle =  middle} 
                :: create_alternative_full_names(tail, last, middle)
        val {first = firstName, last = lastName, middle = middleName} = fullName
        val similarFirstNames = firstName :: rev (get_substitutions2(substitutions, firstName))
    in
        create_alternative_full_names(similarFirstNames, lastName, middleName)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*#2.a*)
fun card_color(card: card) =
    case card of 
        (Clubs, _) => Black
        | (Spades, _) => Black
        | _ => Red

(*#2.b*)
fun card_value(card: card) =
    case card of
        (_, Num num) => num
        | (_, Ace) => 11
        | _ => 10

(*#2.c*)
fun remove_card(cards: card list, card: card, e: exn) =
    let
        fun remove_card_aux(cards: card list, card: card, e: exn, newCards: card list) =
            case cards of
            [] => raise e
            | firstCard :: restOfCards => if (firstCard = card) then rev (newCards) @ restOfCards
                              else remove_card_aux(restOfCards, card, e, firstCard :: newCards)
    in
        remove_card_aux(cards, card, e, [])
    end
    
(*#2.d*)
fun all_same_color(cards: card list) =
    case cards of
    [] => true
    | firstMove :: [] => true
    | first :: second :: restOfCards =>
        case (card_color(first), card_color(second)) of
        (Red, Red) => all_same_color(second :: restOfCards)
        | (Black, Black) => all_same_color(second :: restOfCards)
        | (_, _)=> false

(*#2.e*)
fun sum_cards(cards: card list) =
    let 
        fun sum_cards_aux(cards: card list, aggSum: int) =
            case cards of 
            [] => aggSum
            | firstCard :: restOfCards => sum_cards_aux(restOfCards, aggSum + card_value(firstCard))
    in
        sum_cards_aux(cards, 0)
    end

(*#2.f*)
fun score(cards: card list, goal: int) =
    let 
        val sum = sum_cards(cards)
        val preliminaryScore = if (sum > goal) then  3 * (sum - goal) else goal - sum
    in
        if (all_same_color(cards)) then preliminaryScore div 2 else preliminaryScore
    end

(*#2.g*)
fun officiate(cards: card list, moves: move list, goal: int) =
    let
        fun officiate_aux(cards: card list, moves: move list, goal: int, heldCards: card list) =
            case moves of
            [] => score(heldCards, goal)
            | firstMove :: restOfMoves => 
                (case firstMove of
                Draw =>
                     (case cards of
                     [] => score(heldCards, goal)
                     | firstCard :: restOfCards =>
                        let 
                            val heldCardsWNewCards = firstCard :: heldCards
                        in
                            if (sum_cards(heldCardsWNewCards) > goal) 
                            then score(heldCardsWNewCards, goal)
                            else officiate_aux(restOfCards, restOfMoves, goal, heldCardsWNewCards)
                        end)
                 | Discard card => officiate_aux(cards, restOfMoves, goal, remove_card(heldCards, card, IllegalMove)))
    in
        officiate_aux(cards, moves, goal, [])
    end