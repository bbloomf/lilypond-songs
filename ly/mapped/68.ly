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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #68
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
  \key c \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  g'16[\mf e] |
  c8 c c16[ d] e f |
  g8 g g e |
  a a a8. g16 |
  a8. g16 a b c d |
  
  e4. c16 g |
  c4. g16 e |
  g4. d16 e |
  c4 b'8\rest g16[\p e] |
  c8 c c16[ d] e[ f] |
  
  g8 g g e |
  a8 a a8. g16 |
  a8. g16 a b c d |
  
  e4. c16 g |
  c4. g16 e |
  g4. d16 e |
  c4 b'8\rest \bar"||"
  \times 2/3 { g16[\f a] b } |
  c8 e d c |
  a c4 a8 |
  d4. a8 |
  
  d4. \times 2/3 {g,16[ a b] } |
  c8 e d c |
  a b c8. a16 |
  g8 e c' e, |
  e8 d4 e8 |
  c4. e8 |
  
  d4. a'8 |
  g e c'8. e16 |
  d8 c4 e,8 |
  c4. e8 |
  d4. a'8^\markup\italic"rall." |
  g e e'8.\fermata c16 |
  d8 c4 \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	I wish I was \set ignoreMelismata = ##t in the \unset ignoreMelismata land of cot -- ton,
  Old times there are not for -- got -- ten,
  Look a -- way! Look a -- way! Look a -- way! Dix -- ie Land!
  
  In Dix -- ie Land where I was born in, Ear -- ly on one frost -- y morn -- in’,
  Look a -- way! Look a -- way! Look a -- way! Dix -- ie Land!
  
  Then I wish I was in Dix -- ie, Hoo -- ray!  Hoo -- ray!
  In Dix -- ie Land, I’ll take my stand To live and die in Dix -- ie;
  A -- way, A -- way, A -- way down south in Dix -- ie;
  A -- way, A -- way, A -- way down south in Dix -- ie.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 8
  c8 |
  c c c c16 c |
  c8 c c c |
  c c c8. c16 |
  c8. c16 c g' a a |
  
  g4. e16 e |
  e4. c16 c |
  b4. b16 b |
  c4 s8 c |
  c c c c |
  
  c8 b c c |
  c8 c c8. c16 |
  c8. c16 c16 g' a a |
  
  g4. e16 e |
  e4. c16 c |
  b4. b16 b |
  c4 s8 \bar"||"
  \times 2/3 {g'8 f16 } |
  e8 g g g |
  f f4 f8 |
  fis4. fis8 |
  
  g4. f?8 |
  e8 g f e |
  f f f8. f16 |
  e8 e e c |
  b8 b4  b8 |
  c4. c8 |
  
  b4. f'8 |
  e c e8. g16 |
  f8 e4 c8 |
  c4. c8 |
  b4. f'8 |
  e8 c g'8. g16 |
  f8 e4 \bar"|."
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
  \partial 8
  e,16[ g] |
  e8 e e16[ f] g16 g |
  g8 g g g |
  f f f8. g16 |
  f8. g16 f c' c c |
  
  c4. g16 c |
  g4. e16 g |
  g4. f16 g |
  e4 s8 e16[ g] |
  e8 e e16[ f] g8 |
  
  g8 f e g |
  f8 f f8. g16 |
  f8. g16 f c' c c |
  
  c4. g16 c |
  g4. e16 g |
  g4. f16 g |
  e4 s8 \bar"||"
  \times 2/3 {g8 g16} |
  g8 c b c |
  c a4 c8 |
  d4. d8 |
  
  b4. b8 |
  g c c c |
  c c c8. c16 |
  c8 g g g |
  g8 f4 g8 |
  e4. g8 |
  
  g4. b8 |
  c g g8. c16 |
  b8 g4 g8 |
  g4. g8 |
  g4. b8 |
  c g c8. c16 |
  b8 c4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8
  c,8 |
  c c c c16 d |
  e8 e e c |
  f f f8. e16 |
  f8. e16 f f f f |
  
  c4. c16 c |
  c4. c16 c |
  g4. g16 g |
  c4 d8\rest c |
  c c c c16[ d] |
  
  e8 d c c |
  f8 f f8. e16 |
  f8. e16 f f f f |
  
  c4. c16 c |
  c4. c16 c |
  g4. g16 g |
  c4 d8\rest \bar"||"
  \times 2/3 {g8 g16} |
  c,8 c d e |
  f f4 f8 |
  d4. d8 |
  
  g4. g8 |
  c, c c c |
  f g a8. f16 |
  c8 c c c |
  g g4 g8 |
  c4. c8 |
  
  g4. g8 |
  c c c8. c16 |
  g'8 c,4 c8 |
  e4. c8 |
  g4. g8 |
  c c c8.\fermata e16 |
  g8 c,4 \bar"|."
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
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Dixie"}}
  composer = \markup\oldStyleNum"Dan Emmett (1815–1904)"
  tagline = ""
}}


