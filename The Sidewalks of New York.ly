\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Sidewalks of New York"}}
  composer = \markup\oldStyleNum"Charles B. Lawlor (1852–1925)"
  poet = \markup\oldStyleNum"James W. Blake (1862-1935)"
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
       (padding . -4)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #100
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	b'2 d4 |
  a2 b4 |
  g b2~ |
  b \teeny g4 | \normalsize
  g2 g4 |
  a( g) e |
  
  g2.~ g2 \teeny g4 | \normalsize
  g2 g4 |
  a( g) e |
  d g2~ |
  \tieDashed g b8~ b | \tieSolid
  
  b2 a4 |
  e2 b'4 |
  a2.~ |
  a |
  b2 d4 |
  a2 b4 |
  
  g4 b2~ |
  b2 \teeny \tieDashed g8~ g | \normalsize \tieSolid
  g2 g4 |
  a( g) e |
  g2.~ |
  g2 \teeny \tieDashed g8~ g | \normalsize \tieSolid
  g2 a4 |
  
  g2 e4 |
  \tieDashed << {d4 \normalsize g2} {s32 \teeny d2*1/4 s16. s4 \teeny g4} >> | \normalsize
  c2\fermata b8~ b | \tieSolid |
  b2 a4 |
  e2 fis4 |
  g2.~ |
  g2 b4\rest \bar"||"\break
  
  %chorus
  d2. |
  b |
  a |
  g |
  a2 g4 |
  e2 fis4 |
  g2.~ |
  g2 g4 |
  
  g2 g4 |
  a g e |
  d g2 |
  c b4 |
  b a2 |
  a e4 |
  a2.~ |
  a |
  
  b2 d4 |
  a2 b4 |
  g b2~ |
  b2. |
  g2 g4 |
  a g e |
  g2.~ |
  g2 b4\rest |
  g2 g4 |
  
  a4( g) e |
  d g2 |
  c\fermata b4 |
  b a2 |
  e fis4 |
  g2.~ |
  g2 b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Down in front of Ca -- sey’s __ ""
  Old brown wood -- en stoop, __ ""
  On a sum -- mer’s eve -- ning, __
  We formed a mer -- ry group; __
  
  Boys and girls to -- geth -- er, __ ""
  We would sing and waltz, __
  \set ignoreMelismata = ##t
  While the “gin -- nie” played the or -- "" gan "" \unset ignoreMelismata
  on the __ side -- walks of New York. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  That’s where John -- ny Ca -- sey, __
  And lit -- tle Jim -- my Crowe, __
  With Jak -- ey Krause the bak -- er, __
  Who al -- ways had the dough; __
  
  Pret -- ty Nel -- lie Shan -- non, __
  \set ignoreMelismata = ##t
  With a dude \unset ignoreMelismata as light as cork, __ ""
  First picked up the \set ignoreMelismata = ##t waltz "" step ""
  \unset ignoreMelismata
  on the __ side -- walks of New York. __
  
  East side, West side, __ all a -- round the town, __
  The tots sang “ring a -- round ros -- ie”
  “Lon -- don Bridge is fall -- ing down;”
  Boys and girls to -- geth -- er, __
  Me and Ma -- mie O’ -- Rourke,
  Tripped the light fan -- tas -- tic,
  on the side -- walks of New York. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Things have changed since those times, __ ""
  Some are up in “G,” __ ""
  Oth -- ers, they are wand -- ’rers, __
  \set ignoreMelismata = ##t
  But they all \unset ignoreMelismata feel just like me; __
  
  They’d part with all they’ve got, __
  Could they but once more walk, __
  With their best girl and "" have "" a twirl
  \set ignoreMelismata = ##t
  on the side -- walks of New York. __ _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'2 g4 |
  fis2 fis4 |
  g4 d2~ |
  d2 \teeny g4 | \normalsize
  e2 e4 |
  e2 c4 |
  d2.~ |
  d2 \teeny d4 | \normalsize
  
  e2 e4 |
  e2 c4 |
  b4 d2~ |
  d \tieDashed g8~ g | \tieSolid
  g4( fis) e4 |
  cis2 g'4 |
  fis2.~ |
  fis2. |
  
  g2 g4 |
  fis2 fis4 |
  g4 d2~ |
  d2 \teeny \tieDashed g8~ g | \normalsize \tieSolid
  e2 e4 |
  e2 c4 |
  d2.~ |
  d2 \teeny \tieDashed d8~ d | \normalsize \tieSolid 
  
  e2 e4 |
  cis2 cis4 |
  \tieDashed << {d4 \normalsize b2} {s32 \teeny d2*1/4 s16. s4 \teeny b4} >> | \normalsize
  d2 d8~ d | \tieSolid
  cis2 cis4 |
  c?2 c4 |
  b2.~ |
  b2 s4 \bar"||"
  
  %Chorus (Alto)
  g'2. |
  g |
  fis |
  g2( f4) |
  e2 e4 |
  c2 c4 |
  d2.~ |
  d4( e)  f4 |
  
  e2 e4 |
  e e c |
  b d2 |
  
  e2 d4 |
  cis4 cis2 |
  e2 cis4 |
  d2.~ |
  d |
  
  g2 g4 |
  fis2 fis4 |
  g d2~ |
  d2. |
  
  e2 e4 |
  e e c |
  d2.~ |
  d2 s4 |
  
  e2 e4 |
  e2 c4 |
  d4 b2 |
  
  d2 d4 |
  cis4 cis2 |
  c?2 c4 |
  b2.~ |
  b2 s4 \bar"|."
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
  d2 b4 |
  c2 c4 |
  d b2~ |
  b \teeny b4 | \normalsize
  g2 g4 |
  c2 c4 |
  b2.~ |
  b2 \teeny b4 | \normalsize
  
  g2 g4 |
  c2 g4 |
  g b2~ |
  b \tieDashed d8~ d | \tieSolid
  cis2 cis4 |
  a4( b) cis4 |
  d2.~ |
  d |
  
  d2 b4 |
  c2 c4 |
  d b2~ |
  b \tieDashed \teeny b8~ b | \normalsize \tieSolid
  g2 g4 |
  c2 c4 |
  b2.~ |
  b2 \tieDashed \teeny b8~ b | \normalsize \tieSolid
  
  g2 g4 |
  e2 g4 |
  \tieDashed << {b \normalsize g2} {s32 \teeny b2*1/4 s16. s4 \teeny g4} >> | \normalsize
  gis2 gis8~ gis  | \tieSolid
  g?2 g4 |
  fis2 a4 |
  g2.~ |
  g2 s4 \bar"||"
  
  %Chorus
  b2. |
  d |
  c |
  b |
  c2 c4 |
  g2 a4 |
  b2.~ |
  b2 b4 |
  
  c2 c4 |
  c c g |
  g b2 |
  
  g2 g4 |
  g4 g2 |
  g2 g4 |
  fis2.~ |
  fis |
  
  g2 b4 |
  c2 c4 |
  g b2~ |
  b2. |
  
  g2 g4 |
  c c c4 |
  b2.~ |
  b2 s4 |
  
  g2 g4 |
  c2 g4 |
  b g2 |
  
  gis2 gis4 |
  g?4 g2 |
  fis2 a4 |
  g2.~ |
  g2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g2 g4 |
  d2 d4 |
  b'4 g2~ |
  g2 \teeny g4 | \normalsize
  c,2 c4 |
  c2 c4 |
  
  g'2.~ |
  g2 \teeny g4 | \normalsize
  c,2 c4 |
  c2 c4 |
  g'4 g2~ |
  g \tieDashed g8~ g | \tieSolid
  
  %page2
  e2 g4 |
  g2 a4 |
  d,2.~ |
  d |
  g2 g4 |
  d2 d4 |
  
  b'4 g2~ |
  g \teeny \tieDashed g8~ g | \normalsize \tieSolid
  c,2 c4 |
  c2 c4 |
  g'2.~ |
  g2 \teeny \tieDashed g8~ g | \normalsize \tieSolid
  c,2 c4 |
  
  a2 a4 |
  \tieDashed << {d \normalsize g,2} {s32 \teeny d'2*1/4 s16. s4 \teeny g,4} >> | \normalsize
  e'2 e8~ e | \tieSolid
  e2 e4 |
  d2 d4 |
  g2.~ |
  g2 d4\rest \bar"||"
  
  %Chorus
  g2. |
  g |
  d |
  g2. |
  c,2 c4 |
  c2 d4 |
  g2( fis4 |
  e2) d4 |
  
  c2 c4 |
  c4 c c4 |
  b4 g2 |
  g2 g4 |
  e'4 e2 |
  cis2 a4 |
  d2.~
  d4( e fis) |
  g2 g4 |
  d2 d4 |
  b g2~ |
  g4( a b) |
  c2 c4 |
  c4 c c |
  g'2.~ |
  g2 d4\rest |
  c2 c4 |
  
  c2 c4 |
  g'4 g2 |
  e2\fermata e4 |
  a,4 a2 |
  d2 d4 |
  g,2.~ |
  g2 d'4\rest \bar"|."
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

