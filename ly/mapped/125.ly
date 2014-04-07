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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #125
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
  \slurDashed
}

sopMusic = \relative c' {
	\partial 8
  e16[ fis] |
  g8 b g fis a fis |
  e8 d4 b'4\rest \bar"" e,16[ fis] |
  g8 b g fis a g16[ fis] |
  
  e4. b'4\rest \bar""\break e,16[ fis] |
  g8 b g fis a fis |
  e8 d4 b'4\rest \bar"" b,8 % d8 |
  
  %c16[ b] a[ g] fis[ e] b8 e dis |
  g'8. fis16 e8 b e dis |
  e4. b'4\rest \bar""\break fis8 |
  g g16[ a] b[ c] d8 b g |
  fis d4 b'\rest \bar"" e,16[ fis] |
  
  g8 g16[ a] b[ c] d8 b d |
  e4. b4\rest \bar""\break e16[ e] |
  \slurDashed d8. b16 b8 c8.( b16) a[ g] |
  a fis8. b8\rest b4\rest \bar"" e,16[ fis] |
  
  g8 fis e b'8. a16 g[ fis] |
  e4.~ e4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	It was on a fine sum -- mer’s morn -- ing,
  The birds sweet -- ly tuned on each bough.
  And as I walk’d out for my plea -- sure,
  I saw a maid milk -- ing her cow.
  Her voice so __ en -- chant -- ing mel -- o -- dious,
  Left me quite un -- a -- ble to go,
  \set ignoreMelismata = ##t My __ _ heart it was load -- ed with _ sor -- row,
  \unset ignoreMelismata
  For Col -- leen dhas cru -- then na moe.
  
  
  Col -- leen, Col -- leen, Col -- leen,
  Col -- leen dhas cru -- then na moe.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Then to her I made my ad -- van -- ces;
  “Good mor -- row, most beau -- ti -- ful maid!
  Your beau -- ty my heart so en -- tran -- ces,”
  “Pray Sir, do not ban -- ter,” she said,
  “I’m not such a rare pre -- cious Jew -- el,
  That I should en -- am -- our you so.
  I __ am but a poor \set ignoreMelismata = ##t lit -- tle milk girl.”
  \unset ignoreMelismata
  Says Col -- leen dhas cru -- then na moe.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The In -- dies af -- ford no such Jew -- el
  So bright and trans -- par -- ent -- ly clear,
  Ah! do not add flame to my fu -- el!
  Con -- sent but to love me, my dear.
  Ah! had I __ the lamp of A -- lad -- din,
  \set ignoreMelismata = ##t
  Or the wealth of __ _ the _ Af -- ri -- can shore,
  I would rath -- er be poor _ in a Cot -- tage,
  \unset ignoreMelismata
  With Col -- leen dhas cru -- then na moe.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e8 |
  e e e dis dis dis |
  c8 b4 s e8 |
  e e e dis dis dis |
  e4. s4 e8 |
  e e e dis dis dis |
  c8 b4 s b8 |
  e8. e16 b8 b b a |
  b4. s4 d8 |
  d8 d8 d16[ c] b8 d d |
  d d4 s4 e16[ d] |
  d8 d8 d16[ c] b8 d g |
  g4. s4 g16[ g] |
  \tieDashed g8. d16 e8 e8.~ e16 e[ e] |
  dis16 dis8. s8 s4 e8
  \tieSolid e8 e b dis8. dis16 dis8 |
  e4.~ e4 \bar"|."
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
  e,8 |
  b' g b a fis a |
  g8 g4 s4 e8 |
  b' g b a fis a |
  g4. s4 e8 |
  b' g b a fis a |
  g8 g4 s4 g8 |
  b8. a16 g8 g g fis |
  g4. s4 a8 |
  b8 b16[ a] g8 g g b |
  a8 fis4 s4 e16[ a]
  b8 b16[ a] g8 g g b |
  c4. s4 c16[ c] |
  b8. g16 e8 a8.( b16) c[ a] |
  fis16 a8. s8 s4 g8 |
  b a g fis8. fis16 a8 |
  g4.~ g4
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,8 |
  e8 e e b b b |
  c g4 d'\rest e8 |
  e e e b b b |
  e4. d4\rest e8 |
  
  e e e b b b |
  c g4 d'\rest g8 |
  e8. e16 e8 e e b |
  e4. d4\rest d8 |
  
  g, g g g g g |
  d' d4 d\rest e16[ d] |
  g,8 g g g g g |
  c4. d4\rest c16[ c] |
  \tieDashed g8. g16 gis8 a8.~ a16 a16[ a]|
  b16 b8. d8\rest d4\rest e8 |
  e e e b8. b16 b8 |
  \tieSolid e4.~ e4 \bar"|."
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Pretty Girl Milking Her Cow"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Colleen dhas Cruthen na Moe"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}
