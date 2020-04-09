\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"For he’s a jolly good fellow"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"(We won’t go home until morning)"}}
  composer = \markup\oldStyleNum"Folk Song"
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
       (padding . 1)
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
  g'16[ a] |
  b4 b8 b a b |
  c4. b4 b8 |
  a4 a8 a g a |
  
  b4. g4 \bar"" g16[ a] |
  b4 b8 b a b |
  c4( d8) e4\fermata e8 |
  d4 d8 c4 a8 |
  
  g4.~ g4 \bar"" b16[ c] |
  d4 d8 e4 e8 |
  d4.~ d4 \bar"" b16[ c] |
  d4 d8 e4 e8 |
  
  d4.~ d4 \bar"" g,16[ a] |
  b4 b8 b a b |
  c4. b4 b8 |
  a4 a8 a g a |
  
  b4. g4 \bar"" g16[ a] |
  b4 b8 b a b |
  c4( d8) e4\fermata e8 |
  d4 d8 c4 a8 |
  g4.~ g4 \bar"|."
}
sopWords = \lyricmode {
  For he’s a jol -- ly good fel -- low,
  For he’s a jol -- ly good fel -- low,
  For he’s a jol -- ly good fel -- low,
  And so say all of us; __
  And so say all of us; __
  And so say all of us; __
  For he’s a jol -- ly good fel -- low,
  For he’s a jol -- ly good fel -- low,
  For he’s a jol -- ly good fel -- low,
  And so say all of us. __
}

sopWordsII = \lyricmode {
  We won’t go home un -- til morn -- ing,
  We won’t go home un -- til morn -- ing,
  We won’t go home un -- til morn -- ing,
  Till day -- light doth ap -- pear;
  Till day -- light doth ap -- pear;
  Till day -- light doth ap -- pear;
  We won’t go home un -- til morn -- ing,
  We won’t go home un -- til morn -- ing,
  We won’t go home un -- til morn -- ing,
  Till day -- light doth ap -- pear. __
}

sopWordsIII = \lyricmode {
  %\set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  %\set stanza = #"4. "
}

sopWordsV = \lyricmode {
  %\set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 |
  g4 g8 g g g |
  g4. g4 g8 |
  fis4 fis8 fis e fis |
  
  g4. g4 g8 |
  g4 g8 g g g |
  g4. g4 g8 |
  g4 g8 fis4 fis8 |
  
  g4.~ g4 g8 |
  g4 g8 g4 g8 |
  g4.~ g4 g8 |
  g4 g8 g4 g8 |
  
  g4.~ g4 g8 |
  g4 g8 g g g |
  g4. g4 g8 |
  fis4 fis8 fis e fis |
  
  g4. g4 g8 |
  g4 g8 g g g |
  g4. g4 g8 |
  g4 g8 fis4 fis8 |
  g4.~ g4 \bar"|."
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
  b16[ c] |
  d4 d8 d c d |
  e4. d4 d8 |
  d4 d8 d d d |
  
  d4. b4 b16[ c] |
  d4 d8 d c d |
  c4( b8) c4 c8 |
  b4 b8 d4 c8 |
  
  b4.~ b4 g16[ a] |
  b4 b8 c4 c8 |
  b4.~ b4 g16[ a] |
  b4 b8 c4 c8 |
  
  b4.~ b4 b16[ c] |
  d4 d8 d c d |
  e4. d4 d8 |
  d4 d8 d d d |
  
  d4. b4 b16[ c] |
  d4 d8 d d d |
  c4( b8) c4 c8 |
  b4 b8 d4 c8 |
  b4.~ b4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8 |
  g4 g8 g g g |
  g4. g4 g8 |
  d4 d8 d d d |
  
  g4. g4 g8 |
  g4 g8 g g f |
  e4( d8) c4\fermata c8 |
  d4 d8 d4 d8 |
  
  g4.~ g4 g8 |
  g4 g8 c,4 e8 |
  g4.~ g4 g8 |
  g4 g8 c,4 e8 |
  
  g4.~ g4 g8 |
  g4 g8 g g g |
  g4. g4 g8 |
  d4 d8 d d d |
  
  g4. g4 g8 |
  g4 g8 g g f |
  e4( d8) c4\fermata c8 |
  d4 d8 d4 d8 |
  g4.~ g4 \bar"|."
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
    \tempo 4 = 150
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

