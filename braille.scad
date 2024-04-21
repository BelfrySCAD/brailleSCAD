/////////////////////////////////////////////////////////////////////
// LibFile: braille.scad
//   Create braille dots.  Transcribe text to braille with contractions.
//   Currently English Grade 2 braille (EBAE) is supported.  
// Includes:
//   include <BOSL2/std.scad>
//   include <brailleSCAD/en-us-g2.scad>
//   include <brailleSCAD/braille.scad>
// FileGroup: Braille
// FileSummary: Braille dots and transcription
//////////////////////////////////////////////////////////////////////

// Section: Braille Overview and Printing
//   Braille is a writing system using raised dots that can be read with the fingers
//   by people with impaired vision.  Many different languages can be written using
//   braille.  A "braille cell" is the basic unit of the system.  A braille cell
//   consists of a 3 x 2 arrangement of dots.  The dot positions are identified by
//   numbers:
// Figure(2D,Med,NoAxes): A single braille cell with its dots labeled by their numbers
//   $fn=16;
//   braille([123456],method="dots");
//   color("black")grid_copies(spacing=.092*INCH, n=[2,3])
//      text3d(str(3-$row+$col*3),size=.75,anchor=CENTER,atype="ycenter");
// Continues:
//   One way to refer to a braille character is to list its dots numerically.  In this library
//   you can do that by giving a string like "245" or a number like 1246, which specifies
//   which dots are present in the character.  The number zero indicates an empty cell with no dots.
//   .
//   One might expect a simple mapping between text in a language like English and
//   the braille characters.  But the situation is much more complex.  Braille takes
//   up a lot of space so to make texts and books smaller, the convention of using
//   braille "contractions" was developed.    These contractions replace longer 
//   strings of letters like "ing" with their own symbol, 346 in the case of "ing".  
//   The use of contractions varies with language and may be complicated by rules
//   about when exactly contractions can or cannot be used.
//   .
//   This library takes contraction rules from a standard braille library called liblouis
//   and uses those rules to produce the contracted form ("grade 2") of US English braille,
//   EBAE (English Braile American Edition).  It may eventually be possible to add other
//   languages or UEB (Unified English Braille).
//   .
//   Printing braille dots with a FDM printer is challenging.  The dots are very small
//   so in a horizontal layout, stringing tends to be a severe problem, and the dots develop
//   undesirable sharp tips.  Printing vertically appears to be much easier.  In either case,
//   it seems unlikely that ADA compliant output is possible.  


//include<BOSL2/std.scad>
//include<en-us-g2.scad>
// This code by Adrian Mariano <avm4@cornell.edu>

// John reported that for printing on vertical surfaces, "cone" seemed best and "sphere" a close second
// Drop sphere was OK and cyl was bad.  

// For flat printed John favored "sphere" with "cyl" in second place.  

// Recommended print settings:
//
//   small perimeter speed 5mm/s
//   layer height .1
//   extrusion width .4
//

// https://www.brailleauthority.org/size-and-spacing-braille-characters

// https://greendotsign.com/braille-signage/

// Dot size 0.059 - 0.063 width
//          0.025 - 0.037 height
//    ==>0.03 radius
// Spacing, 0.09 - 0.1
// cell to cell spacing 0.241-0.300
//

_braille_r =        [0.057/2, 0.031];
_braille_height =   [0.019,   0.031];
_braille_dot_sep =  [0.092,   0.095];
_braille_cell_sep = [0.254,   0.27];
_braille_line_sep = [0.4,     0.4];


module _braille_dot(radius,height,style)
{
  if (style=="sphere")
    intersection(){
      scale([1,1,height/radius])sphere(r=radius);
      down(.01)cuboid([3*radius,3*radius, height+1],anchor=BOT);
    }
  if (style=="cone"){
    or = radius*1.1;
    ir = or - height;
    down(.01)cyl(r1=or,r2=ir,h=height,chamfer2=.08,chamfang2=40,anchor=BOT);
  }
  if (style=="cyl")
    down(.01)cyl(r=radius, h=height, rounding2=.2,anchor=BOT);
}

// Section: Braille Modules and Functions


// Module: braille()
// Synopsis: Create braille dots for a text string with transcription to contracted braille.
// SynTags: Geom
// Usage:
//   braille(text, [size=], [style=], [method=], [anchor=], [orient=], [spin=]) {ATTACHENTS};
// Description:
//   Displays a row of braille characters.  The interpretation of the `text` argument depends on the specified `method`.
//   If the `method` is "dots" then `text` is a list of numbers or text string numbers
//   that list the dots of each character.  These numbers are the standard numerical
//   representation for braille, where 1 represents the top left dot, 3 the bottom left dot, 4 the top right dot and
//   6 the bottom right dot.  If `method` is "unicode" then the input must be a unicode text string where each character
//   is either a space or a 6-dot braille unicode character from U+2800 to U+283F.  Finally, `method` can be "un-en-g2",
//   in which case `text` should be an English language text string, which will be transcribed into contracted
//   English US grade 2 braille (EBAE).  Note that the en-us-g2.scad transcription table must be included before
//   braille.scad for transcription to work.  
//   .
//   At the moment, three styles of braille dot output are supported.  The goal is to identify the best one(s) and remove the rest,
//   so available styles may change.  
//   Current styles are "sphere", "cone", and "cyl".  Reportedly when printing using FDM, "cone" is the best for
//   vertical surfaces and sphere is the best for  horizontal surfaces.  
//   .
//   Anchoring is relative to the margins of the text block, not the actual edges of the dots.  This means you can
//   position texts together horizontally or vertically and the spacing should be correct.  
// Arguments:
//   text = input text to display, either English language text, unicode text, or a list of numerical dot values depending on `method`.
//   ---
//   size = size of braille output, either "small" or "large".  Default: "small"
//   method = method for braille display, one of "en-us-g2", "unicode", or "dots".  Default: "en-us-g2"
//   style = style for braille dots.  Default: "sphere".
//   anchor = Translate so anchor point is at the origin (0,0,0).  Default: `CENTER`
//   spin = Rotate this many degrees around the Z axis after anchor.  Default: 0
//   orient = Vector to rotate top towards, after spin.  Default: `UP`
// Examples(Med,NoAxes):
//   braille("⠿⠺⠱ ⠞⠉⠀⠇",method="unicode", $fn=32);
//   braille([123456,1234,123456,15,123456],style="cone",method="dots",$fn=22);
//   braille(["245","145","","12", 134,346,"1","16"],style="cyl", method="dots", $fn=22);
//   braille("This is a test.", $fn=22);
// Examples(Big,NoAxes):
//   braille("The value 44 is smaller than 132.", $fn=22);
//   braille("Look look LOOK LOOk", $fn=22);
//   braille("Hello World! Information.",$fn=22);
// Example(Med,NoAxes): Chaining together rows of braille with left-alignment, where each braile() object is a child of the one before.  Each row is in a different style.  
//   $fn=22;
//   cuboid([40, 38, 5]){
//     fwd(3)right(4)
//     position(TOP+BACK+LEFT)braille("One",anchor=BOTTOM+BACK+LEFT)
//       position(FWD+LEFT)braille("Two", style="cone", anchor=BACK+LEFT)
//       position(FWD+LEFT)braille("Three", style="cyl", anchor=BACK+LEFT);
//   }
// Example(Med,NoAxes): Attaching rows of braille to a face of a cube and then chaining rows with center alignment, where each braille() object is a child of the one before.  This orientation of braille prints better on FDM printers.  
//   cuboid([70, 14, 34]){
//     up(15)
//       attach(FRONT)
//         braille("Top line",anchor=BOTTOM+BACK)
//         position(FWD)braille("Middle line",anchor=BACK)
//         position(FWD)braille("Last!",anchor=BACK);      
//   }
module braille(text,method="en-us-g2",size="small",style="sphere",anchor,orient,spin)
{
  legal_styles = ["cyl","cone","sphere"];
  legal_methods = ["dots", "en-us-g2", "unicode"];
  dummy0 = assert(in_list(method, legal_methods), str("method must be one of ",legal_methods))
           assert(in_list(style,legal_styles), str(style,"style must be one of ",legal_styles));
  braille = method=="dots" ? text
          : method=="unicode" ? braille_unicode(text)
          : braille_transcribe(text);
  dummy=  assert(method!="dots" || is_list(text), "text must be a list of numbers or text strings with \"dots\" method")
          assert(in_list(size,["small","large"]),"size must be \"small\" or \"large\"");
  ind = size=="small" ? 0 : 1;
  attachable(anchor=anchor,orient=orient,spin=spin,
             size=[len(braille)*_braille_cell_sep[ind]*INCH, _braille_line_sep[ind]*INCH, _braille_height[ind]*INCH]){
  down(_braille_height[ind]*INCH/2)
  back(_braille_dot_sep[ind]*INCH)
  left(_braille_dot_sep[ind]/2*INCH+(len(braille)-1)*_braille_cell_sep[ind]/2*INCH)
    for(i=[0:1:len(braille)-1])
      if (braille[i] && braille[i]!="0") // skip if zero or empty string (space)
        right(i*_braille_cell_sep[ind]*INCH)
          for(ch=str(braille[i])){
            assert(len(search(ch,"123456"))>0, str("Got invalid digit, \"",ch, "\", in braille dots."));
            let(n=ord(ch)-ord("1"))
            translate(_braille_dot_sep[ind]*INCH*[floor(n/3),-n%3])
               _braille_dot(_braille_r[ind]*INCH, _braille_height[ind]*INCH,style);
          }
  children();
  }
}


// Function: braille_unicode()
// Synopsis: Convert unicode braille to a braille dots list
// SynTags: Dots
// Usage:
//   braille_dots = braille_unicode(str);
// Description:
//   Converts a string of braille unicode (which may include ASCII space characters)
//   into a list of braille dot numbers.  This output is suitable
//   for display with {{braille()}} using the "dots" method.  
// Example:
//   dots = braille_unicode("⠿⠺⠱ ⠞⠉⠀⠇");  // Produces [123456, 2456, 156, 0, 2345, 14, 0, 123]  
function braille_unicode(str) = [for(ch=str) _unicode_char(ch)];

function _unicode_dots(ch) =
  ch==0 ? 0
  :
  let(
       vals = [for(i=[5:-1:0]) if (floor(ch/2^i)%2) i+1],
       pows = [for(i=idx(vals)) 10^i]
  )
  pows*vals;
       

function _unicode_char(ch) =
    ch==" " ? 0   // Check for ASCII space
  :
    let(num=ord(ch)-10240)
    assert(num>=0 && num<=63,"Non-braille (6 dot) unicode character found")
    _unicode_dots(num);   


// Sort braille table in descending order by size of text in rules

_sorted_braille_table = select(braille_table,
                               sortidx([for(entry=braille_table) -len(entry[1])]));


/*
begword: replace at beginning of word
contraction: output lettersign then the text
endword: replace at end of word
joinword: replace if text is a word followed by whitespace and a letter, and remove the white space
largesign: replace anywhere they appear.  If two words defined as largesigns follow each other, remove the space between
lowword: replace if text is a word preceeded and followed by whitespace (not punctuation)
midendword: replace in middle or end of word
midword: replace in middle of word
multind
partword: replace anywhere in word (preceeded/followed by letter)
prfword: replace if text is a word or is at the end of a word
sufword: replace if text is a word or is at the beginning of a word
syllable: represent the text by the exact dot pattern given, no contractions
word: replace if surrounded by whitespace and/or punctuation
*/


//  C  "decpoint"
//  C  "digit"
//  C  "litdigit"
//  C  "lowercase"
//  C  "math"
//  C  "noletsignafter" - if this char follows a single letter without a space, don't use a letter sign
//  C  "punctuation"
//  C  "sign"
//  C  "uppercase"


_br_letterclass = str(_br_lowercase,_br_uppercase);

//function _br_downcase(str) =
//  str_join([for(ch=str) let(ind=search(ch,braille_case_table[0])) ind==[] ? ch : braile_case_table[1][ind[0]]]);


function _br_num_length(str,ind=0) =
    ind == len(str) ? 0
  : _br_isdigit(str[ind]) ? 1+_br_num_length(str,ind+1)
  : ind < len(str)-1 && str[ind]=="." && _br_isdigit(str[ind+1]) ? 2+_br_num_length(str,ind+2)
  : 0;

function _str_length_in_class(str, class, start=0, _length=0)=
     start==len(str) || len(search(str[start],class))==0 ? _length
   : _str_length_in_class(str, class, start+1, _length+1);


function _br_extract_sign(table, signName) =
  let(
      ind = search([signName], table, 0,0)[0]
  )
  assert(len(ind)==1)
  table[ind[0]][1];


function _extract_char_table(table, classList, output=[], ind=0) =
    ind==len(table) ? output
  : in_list(table[ind][0], classList) ?
       let(
            entry = [table[ind][1],table[ind][2]]
       )
       _extract_char_table(table, classList, [each output, entry], ind+1)
  : _extract_char_table(table,classList,output,ind+1);
                     
letter_sign = _br_extract_sign(braille_table, "letsign");
number_sign = _br_extract_sign(braille_table, "numsign");
caps_letter = _br_extract_sign(braille_table, "capsletter");
begcapsword = _br_extract_sign(braille_table, "begcapsword");
endcapsword = _br_extract_sign(braille_table, "endcapsword");

function _braille_dots(ch) =
  let( ind=search([ch],braille_char_table,1,0)[0] )
  braille_char_table[ind][1];

function _br_isclass(class,ch) = is_def(ch) && len(search(ch,class))>0;
function _br_iswhite(ch) = _br_isclass(_br_space,ch);
function _br_ispunctuation(ch) = _br_isclass(_br_punctuation,ch);
function _br_isletter(ch) = _br_isclass(_br_letterclass,ch);
function _br_iswordbreak(ch) = _br_iswhite(ch) || _br_ispunctuation(ch);
function _br_isdigit(ch) = _br_isclass(_br_litdigit,ch);

// decpoint "matches when char precedes digit"  but if litdigit takes priority it will never match

function _br_punctuation_scan(text, ind, dir) =
    dir==1 && ind==0 ? false
  : dir==-1 && ind==len(text) ? false
  : _br_iswhite(text[ind]) ? false
  : _br_isletter(text[ind]) || _br_isdigit(text[ind]) ? true
  : _br_punctuation_scan(text, ind+dir, dir);

function _find_table_match(table, text, ind, tabind=0) =
    tabind==len(table) ? [_braille_dots(text[ind]), 1, false]
  : let(pattern=table[tabind][1])
    !substr_match(text,ind, pattern) ? _find_table_match(table,text,ind,tabind+1)  // match failed
  // match succeeded, now check for required context
  : let(
        opcode=table[tabind][0],
        prevchar = text[ind-1],
        nextchar = text[ind+len(pattern)],
        nextnextchar = text[ind+len(pattern)+1],
        wordStart = _br_iswordbreak(prevchar),
        wordEnd = _br_iswordbreak(nextchar),
        letterStart = _br_isletter(prevchar),
        letterEnd = _br_isletter(nextchar),
        digitStart = _br_isdigit(prevchar),
        digitEnd = _br_isdigit(nextchar)
    )
      (opcode=="always")
   || (opcode=="word" && wordStart && wordEnd)          
   || (opcode=="begword" && wordStart && letterEnd)  
   || (opcode=="endword" && letterStart && wordEnd)  
   || (opcode=="lowword" && _br_iswhite(prevchar) && _br_iswhite(nextchar)) 
   || (opcode=="midword" && letterStart && letterEnd)
   || (opcode=="begnum" && wordStart && digitEnd) 
   || (opcode=="midnum" && digitStart && digitEnd)
   || (opcode=="endnum" && digitStart)               // This is supposed to also inhibit use of letter sign
   || (opcode=="midendword" && letterStart && (wordEnd || letterEnd))  
   || (opcode=="partword" && (letterStart || letterEnd))  
   || (opcode=="prfword" && (wordStart || letterStart) && wordEnd) 
   || (opcode=="sufword" && wordStart && (wordEnd || letterEnd))
   || (opcode=="postpunc" && !letterEnd && _br_punctuation_scan(text, ind-1, -1))
   || (opcode=="prepunc" && !letterStart && _br_punctuation_scan(text, ind, 1))
   || (opcode=="syllable")                        // This doesn't actually work
  ? let(
        dots = table[tabind][2]=="=" ? [for(ch=pattern) each _braille_dots(ch)] : table[tabind][2]
    )
    [dots,len(pattern),false]
  : opcode=="contraction" && wordStart && wordEnd ? [[letter_sign, for(ch=pattern) each _braille_dots(ch)],len(pattern),false]
  : opcode=="joinword" && wordStart && _br_iswhite(nextchar) && (_br_isletter(nextnextchar) || isLitdigit(nextnextchar))
     ?  [table[tabind][2],len(pattern)+1,false]
  : opcode=="largesign" ? [table[tabind][2], len(pattern), wordStart && _br_iswhite(nextchar)]
  : opcode=="repeated" ? [table[tabind][2], _br_repeat_length(text,ind,pattern), false]
  : opcode=="decpoint" && digitEnd ? [table[tabind][2],len(pattern),false]
  : _find_table_match(table,text,ind,tabind+1);


function _br_repeat_length(text, ind, pattern, totLen=0) =
    !substr_match(text,ind,pattern) ? totLen
  : _br_repeat_length(text, ind+len(pattern), pattern, totLen+len(pattern));


// Function: braille_transcribe()
// SynTags: Dots
// Synopsis: Transcribe English text into braille dots
// Usage:
//   braille_dots = braille_transcribe(text);
// Description:
//   Transcribes a single line of English text into contracted English US grade 2 braille (EBAE: Englishi Braile: American Edition).
//   The output is a list of numbers
//   specifying braille dots in the usual notation, where the digits 1-6 correspond to the dots in each braille character. 
//   Any unicode braille characters encountered in the in put are also translated to dots. 
// Example:
//   echo(braille_transcribe("This is a test."));  // Returns  [6, 1456, 0, 24, 234, 0, 1, 0, 2345, 15, 34, 256]


// checkCaps is needed because various upper case letters exist with no lower case translation,
// so we would get an infinite loop if we recurse on such characters.  

function braille_transcribe(text, output=[], ind=0, largesign=false, checkCaps=true, check_num=true) =
    // Pad input with spaces to create word boundaries
    ind==0 ? braille_transcribe(str(" ",text, " "), output, 1, largesign, checkCaps, check_num)
  : ind==len(text)-1 ? output
  : let(capslen = !checkCaps ? 0 : _str_length_in_class(text, _br_uppercase, ind))
    capslen==1 ? braille_transcribe(str(" ",downcase(text[ind]),substr(text,ind+1)),
                                   concat(output,caps_letter),1,check_num=check_num)
  : capslen>1 ?   // Sequence of caps starts with begcapsword.  Only if next char is a letter do we emit endcapsword
       let(replacement = braille_transcribe(downcase(substr(text,ind,capslen)),checkCaps=false,check_num=check_num))
       braille_transcribe(text, concat(output, begcapsword, replacement, _br_isletter(text[ind+capslen]) ? endcapsword : []),
                          ind+capslen,check_num=check_num)
  : let(numlen = !check_num ? 0 : _br_num_length(text, ind))
    numlen>0 ?
       let(replacement = braille_transcribe(substr(text,ind,numlen),check_num=false,checkCaps=checkCaps))
       braille_transcribe(text,concat(output,number_sign,replacement,_br_isclass(_br_lowercase,text[ind+numlen]) ? letter_sign : []),
                          ind+numlen, check_num=true,checkCaps=checkCaps)
  :
  let(
      dots_len_largesign = _find_table_match(_sorted_braille_table, text, ind),
      dots=dots_len_largesign[0],
      patlen=dots_len_largesign[1],
      newlargesign=dots_len_largesign[2],
      newoutput = largesign && newlargesign && output[len(output)-2]!=0
                    ? concat(list_head(output), dots)
                    : concat(output, dots)
  )
  braille_transcribe(text, newoutput, ind+patlen, newlargesign || (largesign && dots==[0]),checkCaps=checkCaps, check_num=check_num); 

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// A few tests

//text = "Test 3Com and 1aaa 1AAA or 3.4 or .3 and then.";
//text="12345";
//text="This is a test.";

//echo(braille_transcribe(text));

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Opcodes from Liblouis tables

// echo(unique(column(braille_table,0)));   // Print list of opcodes found in the table

//  Opcodes that appear in the English grade 2 file

//  *  "begnum" - replace characters if at beginning of a number
//  *  "endnum" - replace characters at end of number (don't insert a letter sign)
//  *  "midnum" - replace characters in middle of a number
//     "comp6"= translation of characters in 6-dot computer braille
//     "compbrl" - transcribe entire sequence character by character (computer braille)
//     "replace" = replace wherever it appears
//  *  "always"
//  *  "begword"
//  *  "contraction"
//  *  "endword"
//  *  "joinword"
//  *  "largesign"
//  *  "lowword"
//  *  "midendword"
//  *  "midword"
//  *  "partword"
//  *  "prfword"
//  *  "sufword"
//  *  "word"
//  *  "postpunc" - replace characters if they are part of punctuation and word end
//  *  "prepunc" - replace characters if they are part of punctuation at word start
//  *  "repeated" = Replace with dots and ignore any consecutive repeats of sequence
//  ?  "syllable"
//  C  "hyphen"
//  C  "decpoint"
//  C  "digit"
//  C  "litdigit"
//  C  "lowercase"
//  C  "math"
//  C  "noletsignafter" - if this char follows a single letter without a space, don't use a letter sign
//  C  "punctuation"
//  C  "sign"
//  C  "space"
//  C  "uppercase"
//  C  "base" - defines association of upper and lower case...possible other uses in other languages?  
//  X  "begemph"
//  X  "begemphphrase"
//  X  "begemphword"
//  X  "emphclass"
//  X  "endemph"
//  X  "endemphphrase"
//  X  "endemphword"
//  X  "lenemphphrase"
//  X  "multind" - only for back translator, so remove in perl code
//  m  "begcapsword" - dot pattern indicating that word or rest of word is all caps
//  m  "begcomp" - dot pattern showing start of computer braille
//  m  "endcapsword" - dot pattern indicating that all caps sequence ended in middle of word
//  m  "endcomp" - dot pattern ending computer braille
//  m  "letsign" - letter sign
//  m  "numsign" - number sign
//  m  "capsletter" = modeletter uppercase


/*
One thing puzzled me.  I thought the dots for a capital letter was dot
6, but in the file, there are a bunch of comp6 entries for capital
letters and they give things like A as 456-1.  Why is that?
*/


