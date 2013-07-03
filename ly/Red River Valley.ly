\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Red River Valley"}}
  composer = \markup\oldStyleNum"Traditional"
  poet = \markup\oldStyleNum"Traditional"
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
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
  \tieDashed
}

sopMusic = \relative c' {
	\partial 4
  d8 g |
  b4 b8 a g4 a8 g |
  e g4. b4\rest \bar"" d,8 g |
  b4 g8 b d4 c8 b |
  a2. \bar"" d8 c |
  b4 b8 a g4 a8 b |
  d c4. b4\rest \bar"" e,8 ees |
  d4 fis8 g a4 b8 a |
  g2. \bar":|"
  %\break
  
  
  d8 g |
  b4 b8 a g4 a8 g |
  e g4. b4\rest \bar"" d,8 g |
  b4 g8 b d4 c8 b |
  a2. \bar"" \break d8 c |
  b4 b8 a g4 a8 b |
  d c4. b4\rest \bar"" e,8 ees |
  d4 fis8 g a4 b8 a |
  g2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  %\set ignoreMelismata = ##t
	From this val -- ley they say you are go -- ing.
  We will miss your bright eyes and sweet smile,
  For they say you are tak -- ing the sun -- shine
  That has bright -- ened our path -- way a while.
}

sopWordsII = \lyricmode {
  %\set stanza = #"Chorus "
  %\set ignoreMelismata = ##t
  I’ve been think -- ing a long time, my dar -- ling!
  Of the sweet words you nev -- er would say,
  Now a -- las, must the fond hopes all van -- ish?
  For they say you are go -- ing a -- way.
  
  
  
  \dropLyricsXI
  Come and sit by my side if you love me.
  Do not hast -- en to bid me a -- dieu.
  \raiseLyrics
  Just re -- mem -- ber the Red Riv -- er Val -- ley,
  And the cow -- boy who loved you so true.
}

sopWordsIII = \lyricmode {
  \set stanza = "2. "
  I have prom -- ised you dar -- ling that nev -- er,
  Shall the words from my lips cause you pain
  And my life it shall be yours for -- ev -- er,
  If you on -- ly will love me a -- gain.
}

sopWordsIV = \lyricmode {
  When you think of the val -- ley you’re leav -- ing,
  Oh! how lone -- ly and drear it would be,
  When you think of the fond heart you’re break -- ing,
  And the pain you are caus -- ing to me.
}

sopWordsV = \lyricmode {
}

altoMusic = \relative c' {
  b8 b |
  d4 d8 c d4 f8 f |
  e8 e4. s4 d8 d |
  
  d4 d8 d g4 g8 g |
  \slurSolid fis4( g fis) g8 e |
  g4 g8 d d4 f8 f |
  e8 e4. s4 bes8 bes |
  b?4 d8 d d4 g8 fis |
  d4( e d) \bar":|"
  
  
  
  b8 b |
  d4 dis8 dis e4 f8 f |
  e e4. s4 d8 d |
  
  d4 d8 g gis4 gis8 gis |
  g?2( fis4) g8 e |
  g4 g8 d d4 f8 f |
  
  e e4. s4 bes8 bes |
  b?4 d8 d d4 g8 fis |
  d4( e d) \bar"|."
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
  d,8 d |
  g4 g8 g b4 b8 b |
  g8 c4. s4 g8 b |
  
  g4 b8 g b4 e8 d |
  c2. b8 c |
  d4 d8 c b4 b8 g |
  
  g8 g4. s4 g8 g |
  g4 a8 b c4 d8 c |
  \slurSolid b4( c b) \bar":|"
  
  
  
  g8 g |
  g4 a8 b b4 b8 b |
  g c4. s4 g8 b |
  
  g4 g8 b b4 e8 d |
  cis2( c4) b8 c |
  d4 d8 c b4 b8 g |
  
  g g4. s4 g8 g |
  g4 a8 b c4 d8 c |
  b4( c b) \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g,8 g |
  g4 g8 g g4 g8 g |
  c c4. d4\rest b8 g |
  
  g4 g'8 g g4 g8 g |
  \slurSolid d4( e8[ ees] d4) g8 g |
  g4 g8 g g4 g,8 g |
  
  c c4. d4\rest cis8 cis |
  d4 d8 d d4 d8 d |
  g2. \bar":|"
  
  
  
  g8 g |
  g4 fis8 fis e4 d8 d |
  c c4. d4\rest b8 g |
  
  g4 f'8 f e4 e8 e |
  \slurSolid e4( ees d) g8 g |
  g4 g8 g g4 g,8 g |
  
  c8 c4. d4\rest cis8 cis |
  d4 d8 d d4 d8 d |
  g2. \bar"|."
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
    \tempo 4 = 90
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
