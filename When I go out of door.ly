\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"When I go out of door"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 120))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #118
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  \repeat unfold 2 {
    d8 |
    b'4 b8 b4 g8 |
    d4. b'4\rest d,8 |
    b'4 b8 b4 g8 |
    a4. b4\rest a8 |
    c d e e d c |
    
    b c d d c b |
    a b c b4 a8 |
    g4. \bar"" b4\rest \bar""
  } d,8 |
  d'4 d8 d4 d8 |
  d4. b4\rest d,8 |
  d' d d d4 cis8 |
  
  c?4. b4\rest c8 |
  c d e e d c |
  b c d d c b |
  a a gis a4 b8 |
  g4. b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  When I go out of door,
  Of dam -- o -- zels a score,
  (All sigh -- ing and burn -- ing,
  And cling -- ing and yearn -- ing)
  Will fol -- low me as be -- fore.
  
  I shall, with cul -- tured taste,
  Dis -- tin -- guish gems from paste,
  And “High did -- dle did -- dle”
  Will rank as an id -- yll,
  If I __ _ pro -- nounce it chaste!
  
  A most in -- tense young man,
  A soul -- _ ful -- eyed young man,
  An ul -- tra -- po -- et -- ic -- al,
  su -- per -- æs -- thet -- ic -- al,
  Out of the way young man!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Con -- ceive me, if you can,
  An ev -- ’ry -- day young man;
  A com -- mon place type,
  With a stick and a pipe,
  And a half -- _ bred black -- and -- tan.
  
  Who thinks sub -- ur -- ban “hops,”
  More fund than “Mon -- day Pops.”
  Who’s fond of his din -- ner,
  And does -- n’t get thin -- ner
  On bot -- _ tled beer and chops.
  
  A com -- mon -- place young man—
  A mat -- ter -- of -- fact young man—
  A stea -- dy and stol -- id -- y,
  jol -- ly Bank -- hol -- i -- day,
  Ev -- e -- ry -- day young man.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  A Jap -- a -- nese young man—
  A blue and white young man—
  Fran -- ces -- ca di Ri -- mi -- mi,
  mi -- mi -- ny, prim -- i -- ny,
  \markup\italic Je -- \markup\italic ne -- \markup\italic sais -- \markup\italic quoi young man.
  
  A chance -- ry Lane young man—
  A Somer -- set House young man,—
  A ve -- ry de -- lec -- ta -- ble,
  High -- ly re -- spec -- ta -- ble
  Three -- pen -- ny -- bus young man!
  
  Con -- ceive me, if you can,
  A crot -- chet -- y, cracked young man,
  An ul -- tra po -- et -- ic -- al, su -- per -- æs -- thet -- ic -- al,
  Out -- of -- the -- way young man!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  A pallid and thin young man—
  A haggard and lank young man—
  A green -- e -- ry -- yal -- le -- ry,
  Gros -- ve -- nor Gal -- le -- ry,
  Foot -- in -- the -- grave young man!
  
  A Sewell and Cross young man—
  A Howell and James young man—
  A push -- ing young par -- ti -- cle—
  what’s the next ar -- ti -- cle—
  Wa -- ter -- loo House young man!
  
  Con -- ceive me, if you can,
  A mat -- ter -- of -- fact young man,
  An al -- _ pha -- bet -- ic -- al,
  a -- _ rith -- met -- ic -- al,
  Ev -- e -- ry -- day young man!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    d8 |
    g4 g8 g4 d8 |
    d4. s4 d8 |
    g4 g8 g4 d8 |
    fis4. s4 fis8 |
    a8 b c c b a |
    
    g a b b a g |
    fis fis fis fis4 fis8 |
    g4. s4
  }
  d8 |
  b'4 b8 b4 b8 |
  b4. s4 d,8 |
  b' b b b4 ais8 |
  
  a?4. s4 a8 |
  a b c c b a |
  g a b b a g |
  fis fis eis fis4 fis8 |
  g4. s4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \repeat unfold 2 {
    b8 |
    d4 d8 d4 b8 |
    b4. s4 b8 |
    d4 d8 d4 b8 |
    d4. s4 d8 |
    d d d d d d |
    
    d d d d d d |
    c b a d4 c8 |
    b4. s4
  }
  b8 |
  d4 d8 d4 d8 |
  d4. s4 b8 |
  d d d d4 e8 |
  
  fis4. s4 d8 |
  d8 d d d d d |
  d d d d d d |
  c c c c4 d8 |
  b4. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    g8 |
    g4 g8 g4 g8 |
    g4. d4\rest g8 |
    g4 g8 g4 g8 |
    d4. d4\rest d8 |
    fis fis d fis fis d |
    
    g g g g g g |
    d d d d4 d8 |
    g4. d4\rest 
  }
  g8 |
  g4 g8 g4 g8 |
  g4. d4\rest g8 |
  g g g g4 g8 |
  
  d4. d4\rest d8 |
  d d d d d d |
  g g g g g g |
  d d d d4 d8 |
  g4. d4\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}

