﻿\version "2.14.2"
\include "util.ly"
\header {
  title = ""
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  ragged-last-bottom = ##t
  ragged-bottom = ##t
  two-sided = ##t
  ragged-right = ##f
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.5\in
  bottom-margin = 0.75\in
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 23) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 23 20))) }
\markup\vspace #4
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #70 \smallCapsOldStyle"Songs"}}
\markup\vspace #0.75
\markup\fill-line \center-align {\abs-fontsize #35 \italic"from the"}
\markup\vspace #0.75
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #70 \smallCapsOldStyle"Public Domain"}}
\markup\vspace #10
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"selected, transcribed, and edited"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #24 \italic"by"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"benjamin bloomfield"}
\markup\vspace #9
\markup{\abs-fontsize #12 {First edition, \smallCapsOldStyle"june 2013"}}
\markup\vspace #0.1
\markup{\abs-fontsize #12 "This work is free of known copyright restrictions."}
\pageBreak
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #32 \smallCapsOldStyle"Contents"}}
\markup\vspace #2

\markup{{\override #'(line-width . 41.3) \override #'(word-space . 0) \column{
{\page-link #143 {\fill-with-pattern #0.1 #CENTER . "Abide with me" \smallCapsOldStyle 139}}
{\page-link #109 {\fill-with-pattern #0.1 #CENTER . "America" \smallCapsOldStyle 105}}
{\page-link #58 {\fill-with-pattern #0.1 #CENTER . "America the Beautiful" \smallCapsOldStyle 54}}
{\page-link #47 {\fill-with-pattern #0.1 #CENTER . "Am I Not Fondly Thine Own" \smallCapsOldStyle 43}}
{\page-link #49 {\fill-with-pattern #0.1 #CENTER . "Annie Laurie" \smallCapsOldStyle 45}}
{\page-link #43 {\fill-with-pattern #0.1 #CENTER . "The Ash Grove" \smallCapsOldStyle 39}}
{\page-link #44 {\fill-with-pattern #0.1 #CENTER . "The Battle Hymn of the Republic" \smallCapsOldStyle 40}}
{\page-link #139 {\fill-with-pattern #0.1 #CENTER . "The Battle Prayer" \smallCapsOldStyle 135}}
{\page-link #113 {\fill-with-pattern #0.1 #CENTER . "The bell doth toll (Round)" \smallCapsOldStyle 109}}
{\page-link #144 {\fill-with-pattern #0.1 #CENTER . "Be Still, My Soul" \smallCapsOldStyle 140}}
{\page-link #91 {\fill-with-pattern #0.1 #CENTER . "The Birds’ Ball" \smallCapsOldStyle 87}}
{\page-link #81 {\fill-with-pattern #0.1 #CENTER . "The Blue Bells of Scotland" \smallCapsOldStyle 77}}
{\page-link #66 {\fill-with-pattern #0.1 #CENTER . "Boating Song" \smallCapsOldStyle 62}}
{\page-link #38 {\fill-with-pattern #0.1 #CENTER . "Bonnie Doon" \smallCapsOldStyle 34}}
{\page-link #88 {\fill-with-pattern #0.1 #CENTER . "Bride Bells" \smallCapsOldStyle 84}}
{\page-link #124 {\fill-with-pattern #0.1 #CENTER . "Brightly dawns our wedding day" \smallCapsOldStyle 120}}
{\page-link #122 {\fill-with-pattern #0.1 #CENTER . "A British Tar" \smallCapsOldStyle 118}}
{\page-link #53 {\fill-with-pattern #0.1 #CENTER . "By the Sad Sea Waves" \smallCapsOldStyle 49}}
{\page-link #108 {\fill-with-pattern #0.1 #CENTER . "A Capital Ship" \smallCapsOldStyle 104}}
{\page-link #127 {\fill-with-pattern #0.1 #CENTER . "Catch Round the Table (Now we are met)" \smallCapsOldStyle 123}}
{\page-link #137 {\fill-with-pattern #0.1 #CENTER . "Come, Thou Fount of Every Blessing" \smallCapsOldStyle 133}}
{\page-link #22 {\fill-with-pattern #0.1 #CENTER . "Come again, sweet love" \smallCapsOldStyle 18}}
{\page-link #73 {\fill-with-pattern #0.1 #CENTER . "Come Follow (Round)" \smallCapsOldStyle 69}}
{\page-link #73 {\fill-with-pattern #0.1 #CENTER . "Come Follow Me Merrily (Round)" \smallCapsOldStyle 69}}
{\page-link #111 {\fill-with-pattern #0.1 #CENTER . "Come Let Us All A-Maying Go (Round)" \smallCapsOldStyle 107}}
{\page-link #40 {\fill-with-pattern #0.1 #CENTER . "Could I a maiden find" \smallCapsOldStyle 36}}
{\page-link #128 {\fill-with-pattern #0.1 #CENTER . "The criminal cried" \smallCapsOldStyle 124}}
{\page-link #77 {\fill-with-pattern #0.1 #CENTER . "Darby and Joan" \smallCapsOldStyle 73}}
{\page-link #46 {\fill-with-pattern #0.1 #CENTER . "De Brevitate Vitæ (Gaudeamus Igitur)" \smallCapsOldStyle 42}}
{\page-link #120 {\fill-with-pattern #0.1 #CENTER . "The Distant Shore" \smallCapsOldStyle 116}}
{\page-link #60 {\fill-with-pattern #0.1 #CENTER . "Dixie" \smallCapsOldStyle 56}}
{\page-link #52 {\fill-with-pattern #0.1 #CENTER . "Dreaming of Home and Mother" \smallCapsOldStyle 48}}
{\page-link #76 {\fill-with-pattern #0.1 #CENTER . "Dublin Bay" \smallCapsOldStyle 72}}
{\page-link #89 {\fill-with-pattern #0.1 #CENTER . "Ego sum pauper (Round)" \smallCapsOldStyle 85}}
{\page-link #42 {\fill-with-pattern #0.1 #CENTER . "Ein Prosit" \smallCapsOldStyle 38}}
{\page-link #94 {\fill-with-pattern #0.1 #CENTER . "Fairy Belle" \smallCapsOldStyle 90}}
}
\hspace #0.01 \path #0.1 #'((moveto 0 1.4) (lineto 0 -105.1)) \hspace #0.01 \override #'(line-width . 41.3) \override #'(word-space . 0) \column {
{\page-link #29 {\fill-with-pattern #0.1 #CENTER . "The Flight of Love" \smallCapsOldStyle 25}}
{\page-link #117 {\fill-with-pattern #0.1 #CENTER . "The Flowers that Bloom in the Spring" \smallCapsOldStyle 113}}
{\page-link #37 {\fill-with-pattern #0.1 #CENTER . "Flow Gently, Sweet Afton" \smallCapsOldStyle 33}}
{\page-link #107 {\fill-with-pattern #0.1 #CENTER . "For he’s a jolly good fellow" \smallCapsOldStyle 103}}
{\page-link #89 {\fill-with-pattern #0.1 #CENTER . "Gaudeamus Hodie (Round)" \smallCapsOldStyle 85}}
{\page-link #46 {\fill-with-pattern #0.1 #CENTER . "Gaudeamus Igitur (De Brevitate Vitæ)" \smallCapsOldStyle 42}}
{\page-link #75 {\fill-with-pattern #0.1 #CENTER . "The Girl I Left Behind Me" \smallCapsOldStyle 71}}
{\page-link #136 {\fill-with-pattern #0.1 #CENTER . "Glorious Things of Thee Are Spoken" \smallCapsOldStyle 132}}
{\page-link #140 {\fill-with-pattern #0.1 #CENTER . "Glory be to Jesus" \smallCapsOldStyle 136}}
{\page-link #103 {\fill-with-pattern #0.1 #CENTER . "God be with you till we meet again" \smallCapsOldStyle 99}}
{\page-link #142 {\fill-with-pattern #0.1 #CENTER . "God so loved the world" \smallCapsOldStyle 138}}
{\page-link #101 {\fill-with-pattern #0.1 #CENTER . "Good Night Ladies" \smallCapsOldStyle 97}}
{\page-link #5 {\fill-with-pattern #0.1 #CENTER . "Hail! Smiling Morn" \smallCapsOldStyle 1}}
{\page-link #99 {\fill-with-pattern #0.1 #CENTER . "The Hand that Holds the Bread" \smallCapsOldStyle 95}}
{\page-link #97 {\fill-with-pattern #0.1 #CENTER . "Happy Hours at Home" \smallCapsOldStyle 93}}
{\page-link #96 {\fill-with-pattern #0.1 #CENTER . "Hard Times" \smallCapsOldStyle 92}}
{\page-link #38 {\fill-with-pattern #0.1 #CENTER . "Hark! the vesper hymn is stealing" \smallCapsOldStyle 34}}
{\page-link #40 {\fill-with-pattern #0.1 #CENTER . "The Harp that Once Through Tara’s Halls" \smallCapsOldStyle 36}}
{\page-link #63 {\fill-with-pattern #0.1 #CENTER . "The Hazel Dell" \smallCapsOldStyle 59}}
{\page-link #35 {\fill-with-pattern #0.1 #CENTER . "The Heart Bowed Down" \smallCapsOldStyle 31}}
{\page-link #115 {\fill-with-pattern #0.1 #CENTER . "He that Will an Alehouse Keep (Round)" \smallCapsOldStyle 111}}
{\page-link #79 {\fill-with-pattern #0.1 #CENTER . "Home Sweet Home" \smallCapsOldStyle 75}}
{\page-link #65 {\fill-with-pattern #0.1 #CENTER . "A Hot Time in the Old Town" \smallCapsOldStyle 61}}
{\page-link #55 {\fill-with-pattern #0.1 #CENTER . "How can I leave thee" \smallCapsOldStyle 51}}
{\page-link #7 {\fill-with-pattern #0.1 #CENTER . "How Lovely Is the Evening (Round)" \smallCapsOldStyle 3}}
{\page-link #34 {\fill-with-pattern #0.1 #CENTER . "I dreamt I dwelt in marble halls" \smallCapsOldStyle 30}}
{\page-link #47 {\fill-with-pattern #0.1 #CENTER . "Integer Vitae" \smallCapsOldStyle 43}}
{\page-link #39 {\fill-with-pattern #0.1 #CENTER . "In the Spring" \smallCapsOldStyle 35}}
{\page-link #16 {\fill-with-pattern #0.1 #CENTER . "It was a lover and his lass" \smallCapsOldStyle 12}}
{\page-link #90 {\fill-with-pattern #0.1 #CENTER . "Jamie’s on the Stormy Sea" \smallCapsOldStyle 86}}
{\page-link #72 {\fill-with-pattern #0.1 #CENTER . "Jenny the Flower of Kildare" \smallCapsOldStyle 68}}
{\page-link #138 {\fill-with-pattern #0.1 #CENTER . "Jesus, Lover of my soul" \smallCapsOldStyle 134}}
{\page-link #133 {\fill-with-pattern #0.1 #CENTER . "Jesus! the very thought of Thee" \smallCapsOldStyle 129}}
{\page-link #36 {\fill-with-pattern #0.1 #CENTER . "John Anderson, my jo" \smallCapsOldStyle 32}}
{\page-link #114 {\fill-with-pattern #0.1 #CENTER . "Johnny Sands" \smallCapsOldStyle 110}}
{\page-link #32 {\fill-with-pattern #0.1 #CENTER . "Killarney" \smallCapsOldStyle 28}}
}}}
\markup{{\override #'(line-width . 41.3) \override #'(word-space . 0) \column{
{\page-link #118 {\fill-with-pattern #0.1 #CENTER . "La ci darem la mano" \smallCapsOldStyle 114}}
{\page-link #140 {\fill-with-pattern #0.1 #CENTER . "Lead Kindly Light" \smallCapsOldStyle 136}}
{\page-link #11 {\fill-with-pattern #0.1 #CENTER . "Let Us Sing (The Waits)" \smallCapsOldStyle 7}}
{\page-link #71 {\fill-with-pattern #0.1 #CENTER . "A Life on the Ocean Wave" \smallCapsOldStyle 67}}
{\page-link #92 {\fill-with-pattern #0.1 #CENTER . "Listen to the Mocking Bird" \smallCapsOldStyle 88}}
{\page-link #86 {\fill-with-pattern #0.1 #CENTER . "The Little Tin Soldier" \smallCapsOldStyle 82}}
{\page-link #10 {\fill-with-pattern #0.1 #CENTER . "Live we singing" \smallCapsOldStyle 6}}
{\page-link #50 {\fill-with-pattern #0.1 #CENTER . "Loch Lomond" \smallCapsOldStyle 46}}
{\page-link #67 {\fill-with-pattern #0.1 #CENTER . "Long, Long Ago" \smallCapsOldStyle 63}}
{\page-link #61 {\fill-with-pattern #0.1 #CENTER . "The Lorelei" \smallCapsOldStyle 57}}
{\page-link #69 {\fill-with-pattern #0.1 #CENTER . "Love’s Chidings" \smallCapsOldStyle 65}}
{\page-link #33 {\fill-with-pattern #0.1 #CENTER . "Love’s Young Dream" \smallCapsOldStyle 29}}
{\page-link #110 {\fill-with-pattern #0.1 #CENTER . "Maid of Athens" \smallCapsOldStyle 106}}
{\page-link #102 {\fill-with-pattern #0.1 #CENTER . "The March of Prohibition" \smallCapsOldStyle 98}}
{\page-link #127 {\fill-with-pattern #0.1 #CENTER . "Merrily Greet the Morn (Round)" \smallCapsOldStyle 123}}
{\page-link #100 {\fill-with-pattern #0.1 #CENTER . "Merrily Sing" \smallCapsOldStyle 96}}
{\page-link #78 {\fill-with-pattern #0.1 #CENTER . "The Midshipmite" \smallCapsOldStyle 74}}
{\page-link #31 {\fill-with-pattern #0.1 #CENTER . "The Minstrel Boy" \smallCapsOldStyle 27}}
{\page-link #131 {\fill-with-pattern #0.1 #CENTER . "Mister Speaker, though ’tis late (Round)" \smallCapsOldStyle 127}}
{\page-link #14 {\fill-with-pattern #0.1 #CENTER . "My bonny lass she smileth" \smallCapsOldStyle 10}}
{\page-link #95 {\fill-with-pattern #0.1 #CENTER . "My Old Kentucky Home" \smallCapsOldStyle 91}}
{\page-link #80 {\fill-with-pattern #0.1 #CENTER . "Nancy Lee" \smallCapsOldStyle 76}}
{\page-link #48 {\fill-with-pattern #0.1 #CENTER . "Night Song" \smallCapsOldStyle 44}}
{\page-link #12 {\fill-with-pattern #0.1 #CENTER . "Now is the month of maying" \smallCapsOldStyle 8}}
{\page-link #127 {\fill-with-pattern #0.1 #CENTER . "Now we are met (Catch Round the Table)" \smallCapsOldStyle 123}}
{\page-link #42 {\fill-with-pattern #0.1 #CENTER . "O Calm of Night" \smallCapsOldStyle 38}}
{\page-link #135 {\fill-with-pattern #0.1 #CENTER . "Ode to Joy" \smallCapsOldStyle 131}}
{\page-link #56 {\fill-with-pattern #0.1 #CENTER . "O Fair Dove, O Fond Dove" \smallCapsOldStyle 52}}
{\page-link #30 {\fill-with-pattern #0.1 #CENTER . "Oft in the stilly night" \smallCapsOldStyle 26}}
{\page-link #141 {\fill-with-pattern #0.1 #CENTER . "Oh, happy is the man that hears" \smallCapsOldStyle 137}}
{\page-link #101 {\fill-with-pattern #0.1 #CENTER . "Oh My Love (Round)" \smallCapsOldStyle 97}}
{\page-link #83 {\fill-with-pattern #0.1 #CENTER . "Old Dog Tray" \smallCapsOldStyle 79}}
{\page-link #93 {\fill-with-pattern #0.1 #CENTER . "The Old Folks at Home" \smallCapsOldStyle 89}}
{\page-link #70 {\fill-with-pattern #0.1 #CENTER . "The Old Musician and His Harp" \smallCapsOldStyle 66}}
{\page-link #59 {\fill-with-pattern #0.1 #CENTER . "The Old Time" \smallCapsOldStyle 55}}
}
\hspace #0.01 \path #0.1 #'((moveto 0 1.4) (lineto 0 -102.1)) \hspace #0.01 \override #'(line-width . 41.3) \override #'(word-space . 0) \column {
{\page-link #64 {\fill-with-pattern #0.1 #CENTER . "On the Banks of the Wabash, Far Away" \smallCapsOldStyle 60}}
{\page-link #41 {\fill-with-pattern #0.1 #CENTER . "O Sole Mio" \smallCapsOldStyle 37}}
{\page-link #8 {\fill-with-pattern #0.1 #CENTER . "Praise of Spring" \smallCapsOldStyle 4}}
{\page-link #82 {\fill-with-pattern #0.1 #CENTER . "Punchinello" \smallCapsOldStyle 78}}
{\page-link #51 {\fill-with-pattern #0.1 #CENTER . "Red is the Rose" \smallCapsOldStyle 47}}
{\page-link #45 {\fill-with-pattern #0.1 #CENTER . "The Roast Beef of Old England" \smallCapsOldStyle 41}}
{\page-link #48 {\fill-with-pattern #0.1 #CENTER . "Robin Adair" \smallCapsOldStyle 44}}
{\page-link #105 {\fill-with-pattern #0.1 #CENTER . "Rule Britannia" \smallCapsOldStyle 101}}
{\page-link #54 {\fill-with-pattern #0.1 #CENTER . "Sailing" \smallCapsOldStyle 50}}
{\page-link #112 {\fill-with-pattern #0.1 #CENTER . "Saint Patrick’s Day" \smallCapsOldStyle 108}}
{\page-link #62 {\fill-with-pattern #0.1 #CENTER . "Santa Lucia" \smallCapsOldStyle 58}}
{\page-link #84 {\fill-with-pattern #0.1 #CENTER . "Saved From the Storm" \smallCapsOldStyle 80}}
{\page-link #68 {\fill-with-pattern #0.1 #CENTER . "Scotch Lassie Jean" \smallCapsOldStyle 64}}
{\page-link #18 {\fill-with-pattern #0.1 #CENTER . "Shoot false love I care not" \smallCapsOldStyle 14}}
{\page-link #106 {\fill-with-pattern #0.1 #CENTER . "The Sidewalks of New York" \smallCapsOldStyle 102}}
{\page-link #23 {\fill-with-pattern #0.1 #CENTER . "Since first I saw your face" \smallCapsOldStyle 19}}
{\page-link #115 {\fill-with-pattern #0.1 #CENTER . "Skating (Round)" \smallCapsOldStyle 111}}
{\page-link #132 {\fill-with-pattern #0.1 #CENTER . "Soldier’s Hymn" \smallCapsOldStyle 128}}
{\page-link #24 {\fill-with-pattern #0.1 #CENTER . "Song of Spring" \smallCapsOldStyle 20}}
{\page-link #134 {\fill-with-pattern #0.1 #CENTER . "The Spacious Firmament on High" \smallCapsOldStyle 130}}
{\page-link #87 {\fill-with-pattern #0.1 #CENTER . "Sweet Genevieve" \smallCapsOldStyle 83}}
{\page-link #110 {\fill-with-pattern #0.1 #CENTER . "The Tailor and the Mouse" \smallCapsOldStyle 106}}
{\page-link #58 {\fill-with-pattern #0.1 #CENTER . "There’s Music in the Air" \smallCapsOldStyle 54}}
{\page-link #130 {\fill-with-pattern #0.1 #CENTER . "Tit-Willow" \smallCapsOldStyle 126}}
{\page-link #21 {\fill-with-pattern #0.1 #CENTER . "Trust" \smallCapsOldStyle 17}}
{\page-link #98 {\fill-with-pattern #0.1 #CENTER . "’Twere vain to tell" \smallCapsOldStyle 94}}
{\page-link #46 {\fill-with-pattern #0.1 #CENTER . "Vive L’Amour" \smallCapsOldStyle 42}}
{\page-link #11 {\fill-with-pattern #0.1 #CENTER . "The Waits (Let Us Sing)" \smallCapsOldStyle 7}}
{\page-link #104 {\fill-with-pattern #0.1 #CENTER . "A Warrior Bold" \smallCapsOldStyle 100}}
{\page-link #126 {\fill-with-pattern #0.1 #CENTER . "When I go out of door" \smallCapsOldStyle 122}}
{\page-link #133 {\fill-with-pattern #0.1 #CENTER . "When I in pain and sorrow moan" \smallCapsOldStyle 129}}
{\page-link #132 {\fill-with-pattern #0.1 #CENTER . "When Jesus Wept (Round)" \smallCapsOldStyle 128}}
{\page-link #74 {\fill-with-pattern #0.1 #CENTER . "When You and I Were Young, Maggie" \smallCapsOldStyle 70}}
{\page-link #116 {\fill-with-pattern #0.1 #CENTER . "Where There’s a Will There’s a Way" \smallCapsOldStyle 112}}
{\page-link #28 {\fill-with-pattern #0.1 #CENTER . "With Horse and Hound" \smallCapsOldStyle 24}}
}}}
\pageBreak
\markup""