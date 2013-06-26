\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.625\in
  outer-margin = 0.375\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #12
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
#(set-global-staff-size 17.5) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.5 20))) }
global = {
  \key g \major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4
  g'4 |
  b4. c8 d4 d |
  c b a a8 b |
  c4 c8 d e4 e8 f |
  
  g2. f8 e |
  d4 b8 c d2~ |
  d4 c8 b c4 a |
  
  g1 |
  r2 r4 d' |
  d d e d |
  c b a a |
  
  %page2
  e'2 cis4 a |
  d2 b4 d |
  g2 e4 c |
  e d c b |
  
  d2 b4 g |
  c b a d |
  d8 c b a g4 g' |
  
  g8 f e d c4 e |
  e8 d c b a4 c |
  b4. b8 a4. a8 |
  
  %page3
  g4 g d'2 |
  b r2 |
  r4 d g2 |
  e4 c e2 |
  
  d4 c2 b4 |
  d2 b4 g |
  c b a d |
  d8 c b a g4 g' |
  
  g8 f e d c4 e |
  e8 d c b a4 c |
  b4. b8 a4. a8 |
  g1\fermata
  \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  It was a lov -- er and his lass,
  
  With a hey, with a ho, and a hey non -- ny no, and a hey __ non -- ny non -- ny no,
  
  That o’er the green corn -- fields did pass,
  In spring -- time, in spring -- time, in spring -- time, the on -- ly pret -- ty ring -- time,
  When birds do sing
  Hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding, 
  Sweet lov -- ers love the spring, in spring -- time, in spring -- time, the on -- ly pret -- ty ring -- time,
  when birds do sing hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding, 
  Sweet lov -- ers love the spring.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Be -- tween the a -- cres of the Rye,
 
  "" \repeat unfold 19 \skip1
  These pret -- ty Coun -- try folks would lie,
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  This Ca -- rol they be -- gan that hour,
 
  "" \repeat unfold 19 \skip1
  How that a life was but a Flow’r,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  And there -- fore take the pres -- ent time,
 
  "" \repeat unfold 19 \skip1
  For love is crown -- ed with the prime,
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c'' {
  g4 |
  g4. a8 b4 a~ |
  a g2 f4 |
  e a4. g8 c4~ |
  
  c8 b g a b4 d8 c |
  b g d'2 c8 b |
  a4 g4. a8( fis4) |
  
  g2. b4 |
  a8 g g2 fis4 |
  g8[ a] b4 c b |
  e4. d8 c b a4~ |
  
  %page2
  a gis a2 |
  r4 fis g2 |
  b r4 c |
  c b a8 g g4~ |
  
  g( fis) g b |
  a g2( fis4) |
  g2 g'4 g8 f |
  
  e d c2 c4 |
  c8 b a g fis4 a~ |
  a8 g g2 fis4 |
  
  %page3
  g4 b a2 |
  g4 g2 fis4 |
  g2 r4 b |
  c c c2 |
  
  b4 a2 g4~ |
  g fis g b |
  a g2( fis4) |
  g2 g'4 g8 f |
  
  e d c2 c4 |
  c8 b a g fis4 a~ |
  a8 g g2 fis4 |
  g1\fermata
  \bar "|."
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
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
}
tenorMusic = \relative c' {
  g'4 |
  g2. d8[ e] fis4 g d d |
  a a8 b c4 c8 d |
  
  e4. f8 g2~ |
  g4 g,8 a b4 c |
  d g c, d |
  
  g,2. g'4 |
  fis g d2 |
  g,4 g c g' |
  a e a, a' |
  
  %page2
  e2 a, |
  r4 d g2 |
  g4 g, c2 |
  c4 d8[ e] f4 g |
  
  d2 g,4 g' |
  fis g( d2) |
  g,4 g' g8 f e d |
  
  c2 c4 c~ |
  c c d a |
  e' g d2 |
  
  %page3
  g,4 g'2 fis4 |
  g g, d'2 |
  g, r4 g |
  c2 c8[ d] e[ f] |
  
  g4 a fis? g |
  d2 g,4 g' |
  fis4 g( d2) |
  g,4 g' g8 f e d |
  
  c2 c4 c~ |
  c c d a |
  e' g d d |
  g,1\fermata
  \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}


altusWordsA = \lyricmode {
  \set stanza = #"1. "
  It was a lov -- er and his lass,
  
  With a hey, __ ho, non -- ny no, non -- ny non -- ny no,
  with a hey non -- ny no,
  
  That o’er the green corn -- fields did pass,
  that o’er the green corn -- fields did pass,
  In spring -- time, the on -- ly pret -- ty ring -- time,
  When birds do sing
  Hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding, 
  lov -- ers love the spring, sweet lov -- ers love the spring,
  the spring, the on -- ly pret -- ty __ ring -- time, when birds do __ sing
  hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding, lov -- ers love the spring.
}

altusWordsB = \lyricmode {
  \set stanza = #"2. "
  Be -- tween the a -- cres of the Rye,
 
  "" \repeat unfold 17 \skip1
  These pret -- ty Coun -- try folks would lie,
  these pret -- ty Coun -- try folks would lie,
}

altusWordsC = \lyricmode {
  \set stanza = #"3. "
  This Ca -- rol they be -- gan that hour,
 
  "" \repeat unfold 17 \skip1
  How that a life was but a Flow’r,
  how that a life was but a Flow’r,
}

altusWordsD = \lyricmode {
  \set stanza = #"4. "
  And there -- fore take the pres -- ent time,
 
  "" \repeat unfold 17 \skip1
  For love is crown -- ed with the prime,
  for love is crown -- ed with the prime,
}

tenorWordsA = \lyricmode {
  \set stanza = #"1. "
  It was a lov -- er and his lass,
  
  With a hey non -- ny non -- ny no, __ with a hey ho non -- ny non -- ny no,
  
  That o’er the green fields, the green corn -- fields did pass,
  In spring -- time, in spring -- time, in spring -- time, the on -- ly ring -- time,
  When birds do sing
  Hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding -- a -- ding,
  Sweet lov -- ers love the spring, in spring -- time, in spring -- time, the on -- ly pret -- ty ring -- time,
  when birds do sing
  Hey ding -- a -- ding -- a -- ding, hey ding -- a -- ding,
  Sweet lov -- ers love the spring.
}

tenorWordsB = \lyricmode {
  \set stanza = #"2. "
  Be -- tween the a -- cres of the Rye,
 
  "" \repeat unfold 16 \skip1
  These pret -- ty Coun -- try, these Coun -- try folks would lie,
}
tenorWordsC = \lyricmode {
  \set stanza = #"3. "
  This Ca -- rol they be -- gan that hour,
 
  "" \repeat unfold 16 \skip1
  How that a life was, a life was but a Flow’r,
}

tenorWordsD = \lyricmode {
  \set stanza = #"4. "
  And there -- fore take the pres -- ent time,
 
  "" \repeat unfold 16 \skip1
  For love is crown -- ed, is crown -- ed with the prime,
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \set Staff.instrument = "Cantus"
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopranosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "sopranosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "sopranosIV"  \lyricsto "sopranos" \sopWordsIV
    
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altusA"  \lyricsto "altos" \altusWordsA
    \new Lyrics = "altusB"  \lyricsto "altos" \altusWordsB
    \new Lyrics = "altusC"  \lyricsto "altos" \altusWordsC
    \new Lyrics = "altusD"  \lyricsto "altos" \altusWordsD
    
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "tenorA"  \lyricsto "tenors" \tenorWordsA
    \new Lyrics = "tenorB"  \lyricsto "tenors" \tenorWordsB
    \new Lyrics = "tenorC"  \lyricsto "tenors" \tenorWordsC
    \new Lyrics = "tenorD"  \lyricsto "tenors" \tenorWordsD
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      %\override VerticalAxisGroup #'staff-affinity = #0
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"It was a lover and his lass"}}
  poet = \markup\oldStyleNum"William Shakespeare (1564–1616)"
  composer = \markup\oldStyleNum"Thomas Morley (1557–1602)"
  tagline = ""
}}



