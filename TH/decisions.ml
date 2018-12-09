module Decisions =
struct
    let spades = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9;10;11;12;] ;;
    let clubs  = [13;14;15;16;17;18;19;20;21;22;23;24;25;] ;;
    let diams  = [26;27;28;29;30;31;32;33;34;35;36;37;38;] ;;
    let hearts = [39;40;41;42;43;44;45;46;47;48;49;50;51;] ;;            

    let isFlush cs = true
    let isNumRank xs k n = true
    let isCare cs = true
    let isSet  cs = true
    let isPair cs = true
    let isFull cs = true
    let isStraight cs = true
end 
