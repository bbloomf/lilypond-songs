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
  first-page-number = #161
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key c \major
  \time 6/8
  %\dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4.
  c'4 g8 |
  e4 c8 c4 e8 |
  g4. e'4 c8 |
  b4 g8 g[ a] b |
  
  c4. e,8[ d] c |
  g'4.\fermata c4 d8 |
  e4.~ e~ |
  e4 e8 \acciaccatura e d4 cis8 |
  
  d4. b4 cis8 |
  d4.~ d~ |
  d4 d8 c4 b8 |
  c4. c4 c8 |
  
  a4 a8 d4 d8 |
  b4. e4 d8 |
  c[ d] e d[ c] b |
  c4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Mis -- ter Speak -- er, though ’tis late,
  Mis -- ter Speak -- er, though ’tis late,
  though ’tis late,
  I must length -- en the de -- bate,
  I must length -- en the de -- bate,
  Mis -- ter Speak -- er, though ’tis late,
  I must length -- en the de -- bate.
}

altoMusic = \relative c' {
  \partial 4.
  r4 r8 |
  c'4 g8 c4 g8 |
  d'4 b8 r4 r8 |
  d4 b8 d4 g,8 |
  
  e'4 c8 g4 c8 |
  b4.\fermata r4 r8 |
  r4 r8 e, f g |
  a[ b] cis] d4 a8 |
  
  d,4. r8 f e |
  d4. d8 e f |
  g[ a] b c4 g8 |
  c,4. c8 d e |
  
  f4 f8 d e f |
  g4 g8 e f g |
  a4 e8 f4 g8 |
  c,4. \bar"|."
}
altoWords = \lyricmode {
  \set stanza = \markup{\dynamic"f " "2. "}
  Ques -- tion, ques -- tion, ques -- tion,
  ques -- tion, ques -- tion, hear him! hear him! hear!
  \set stanza = \markup\dynamic"mp"
  Sir, I shall name you if you stir, if you stir,
  Sir, I shall name you if you stir,
  Sir, I shall name you,
  Sir, I shall name you,
  Sir, I shall name you if you stir.
}
altoWordsII = \lyricmode {
%\markup\italic
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
  \partial 4.
  r4 r8 |
  r4 r8 e'4\ff c8 |
  g'4 d8 g4 e8 |
  g4\fff d8 g4 d8 |
  
  g4 e8 g[ f] e |
  d4.\fermata e4\mp f8 |
  g4 g8 g4. |
  g4 g8 \acciaccatura g f4 e8 |
  
  f4. d4 e8 |
  f4 f8 f4. |
  f4 f8 e4 d8 |
  e4. e4 e8 |
  
  c4 c8 f4 f8 |
  d4. g4 f8 |
  e[ f] g f[ e] d |
  e4. \bar"|."
}

tenorWords = \lyricmode {
  \set stanza = #"3. "
  Or -- der, or -- der, or -- der,
  hear him! hear him! hear him! hear him! hear!
  pray sup -- port the chair,
  pray sup -- port the chair,
  pray sup -- port the chair,
  pray sup -- port the chair,
  Ques -- tion,
  Or -- der, hear him! hear!
  pray sup -- port, sup -- port the chair.
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = first <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics \lyricsto "sopranos" \sopWords
    \new Staff = second <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics \lyricsto "altos" \altoWords
    \new Staff = third <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics \lyricsto "tenors" \tenorWords
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Mister Speaker, though ’tis late"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  composer = \markup\oldStyleNum"Joseph Baildon (d. 1774)"
  tagline = ""
}}


