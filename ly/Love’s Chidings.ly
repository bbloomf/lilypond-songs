\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Love’s Chidings"}}
  composer = \markup\oldStyleNum"Nannie, 1862"
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
       (stretchability . 70))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
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
  \time 3/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	g'4. |
  g8 fis g |
  a4. |
  d4 b8\rest |
  g4. |
  g8 fis g |
  a4. |
  d,8[ e fis] |
  
  g4. |
  b4 b8 |
  c4. |
  e,4 b'8\rest |
  d,4. |
  fis8[ e] fis |
  g4.~ |
  g4 b8\rest \bar"||"\break
  
  b4. |
  b8[ a] b |
  d4( c8) |
  a4. |
  b |
  b8[ c] b |
  a4. |
  d,8[ e fis] |
  g4. |
  b4 b8 |
  c4. |
  e, |
  d |
  fis8[ e] fis |
  g4.~ |
  g4 b8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Why thus do you try me,
  Why thus do you fly me, __
  Why thus de -- ny me,
  Day af -- ter day? __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Thee have I loved dear -- ly,
  Yes, mad -- ly, sin -- cere -- ly, __
  But thou hast near -- ly
  Made Hope grow grey! __
  
  Hast thou no feel -- ing,
  To see me kneel -- ing, __
  My love re -- \set associatedVoice = "altos" veal -- ing,
  \unset associatedVoice
  Day af -- ter day?
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Ah! then must we sev -- er?
  Part -- ed __ _ for -- ev -- er! __
  And wilt thou nev -- er
  Think, love, of me? __
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4. |
  d8 d d |
  fis4. |
  fis4 s8 |
  d4. |
  d8 d d |
  fis4. |
  d4. |
  
  g4. |
  f4 f8 |
  e4. |
  c4 s8 |
  b4. |
  c4 d8 |
  d4.~ |
  d4 s8 |
  
  g4. |
  g8[ fis] g |
  a4. |
  fis |
  g |
  g4 g8 |
  fis4. |
  d8[ cis c] |
  b4( d8) |
  g4 g8 |
  g4. |
  e4( c8) |
  b4. |
  d4 d8 |
  d4.~ |
  d4 s8 \bar"|."
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
  b4. |
  b8 b b |
  c4. |
  c4 s8 |
  b4. |
  b8 b b |
  c4. |
  c4. |
  
  b4. |
  d4 d8 |
  c4. |
  c4 s8 |
  b4. |
  a4 c8 |
  b4.~ |
  b4 s8 |
  
  d4. |
  d4 d8 |
  d4. |
  d |
  d |
  d8[ e] d |
  c4. |
  a |
  g4( b8) |
  d4 f8 |
  e4. |
  c4( g8) |
  g4. |
  a4 c8 |
  b4.~ |
  b4 s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4. |
  g8 g g |
  d4. |
  d4 d8\rest |
  g4. |
  g8 g g |
  d4. |
  d4. |
  
  g4. |
  g4 g8 |
  c,4. |
  c4 d8\rest |
  d4. |
  d4 d8 |
  g,4.~ |
  g4 d'8\rest |
  
  g4. |
  g4 g8 |
  fis4. |
  d |
  g |
  g4 g8 |
  d4. |
  d |
  g |
  g4 g,8 |
  c4. |
  c |
  d |
  d4 d8 |
  g4.~ |
  g4 d8\rest \bar"|."
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
    \tempo 4 = 105
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


