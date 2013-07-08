\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Hot Time in the Old Town"}}
  poet = \markup\oldStyleNum"Joseph Hayden"
  composer = \markup\oldStyleNum"Theodore August Metz, 1896"
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
       (padding . 3)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.95\in
  outer-margin = 0.7\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	b'2 b |
  b2. ais8. b16 |
  c4 b ais b |
  g2. b4\rest |
  d2 d |
  d2. cis8.[ d16] |
  
  e4 d cis d |
  b \bar"" d, e g |
  b2 b |
  b2. \bar"" ais8. b16 |
  c4 b ais b |
  
  g g fis g |
  b a2 d,8 d |
  b'4 a2 d,4 |
  g1 \bar"|."
}
sopWords = \lyricmode {
%{  \set stanza = #"1. "
  \set ignoreMelismata = ##t 
  When you hear that the preach -- ing does be -- gin
  Bend down low for to drive a -- way your sin
  and when you gets re -- ligion
  You _ want to shout and sing
  there’ll be a hot time in the old town to -- night. % my ba -- by
%}
}

sopWordsII = \lyricmode {
%  \set stanza = \markup\smallCapsOldStyle" refrain "
	When you hear dem a bells go ding, ling, ling,
  All join ’round
  And sweet -- ly you must sing,
  And when the verse is through,
  In the cho -- rus all join in,
  There’ll be a hot time in the old town to -- night.
}

sopWordsIII = \lyricmode {
%{  \set stanza = #"2. "
  \set ignoreMelismata = ##t 
  Please oh, please, oh, _ do not let me fall
  You’re all mine and I love you best of all
  and you must be my man
  Or I’ll have no man at all
  there’ll be a hot time in the old town to -- night. % my ba -- by
%}
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'2 g |
  g2. g8. g16 |
  g4 g g g |
  g2. s4 |
  b2 b |
  b2. ais8.[ b16] |
  
  c4 b ais b |
  g d e g |
  g2 g |
  g2. g8. g16 |
  g4 g g g |
  
  g g fis g |
  fis fis2 d8 d |
  fis4 fis2 d4 |
  d1 \bar"|."
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
  d2 d |
  d2. cis8. d16 |
  e4 d cis d |
  b2. s4 |
  d2 d |
  d2. e8.[ d16] |
  
  c4 d e d |
  d d, e g |
  d'2 d |
  d2. cis8. d16 |
  e4 d cis d |
  
  b g fis g |
  d' c2 d,8 d |
  d'4 c2 d,4 |
  b'1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g2 g |
  g2. g8. g16 |
  g4 g g g |
  g2. d4\rest |
  g2 g |
  g2. g4 |
  
  g4 g g g |
  g d e g |
  g2 g |
  g2. g8. g16 |
  g4 g g g |
  
  g g fis g |
  d d2 d8 d |
  d4 d2 d4 |
  g,1 \bar"|."
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
    \tempo 4 = 240
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
