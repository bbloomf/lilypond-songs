\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Old Musician and His Harp"}}
  poet = \markup\oldStyleNum"William S. Pitts (1830–1918)"
  composer = \markup\oldStyleNum"H. M. Higgins (1820–1897)"
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
  first-page-number = #64
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  ees8 aes |
  c4~ c4. bes8 aes f |
  
  aes2. \bar"" aes8 aes |
  aes4~ aes4. g8 f ees |
  bes'2. \bar"" ees,8 aes |
  c4~ c4.
  bes8 aes f |
  aes2. \bar"" aes8 f |
  
  ees4~ ees4. ees8 ees16[ f] g8 |
  aes2. \bar"" bes8 bes |
  bes4~ bes4. bes8 c16 c8. |
  bes2. \bar"" f8 f |
  f4~ f4. f8 f g |
  
  ees2. \bar"" ees8 aes |
  c4~ c4. bes8 aes f |
  aes2. \bar"" aes8 f' |
  ees4~ ees4. c8 bes c |
  aes2. \bar"||"\break
  
  
  %chorus
  ees'8 ees |
  f4~ f4. f8 ees8. c16 |
  ees2. ees8 f |
  ees4~ ees4. c8 bes aes |
  
  bes2. c8 c |
  c4~ c4. bes8 aes f |
  aes2. aes8 f |
  ees4.^\markup\italic"rit." ees'8 ees4\fermata bes8[ c] |
  aes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Years have come and pass’d a -- way,
  Gold -- en locks have turn’d to gray,
  Gold -- en ring -- lets, once so fair,
  Time has changed to sil -- v’ry hair;
  
  Yes, I’ve neared the riv -- er side,
  Soon I’ll launch up -- on its tide—
  Soon my boat, with noise -- less oar,
  Safe will pass __ to yon -- der shore.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh! those chords with mag -- ic pow’r!
  Take me back to child -- hood’s hour—
  To that cot __ be -- side the sea,
  Where I knelt at moth -- er’s knee;
  
  But that moth -- er, she has gone—
  Calm she sleeps be -- neath the stone,
  While I wan -- der here a -- lone,
  Sigh -- ing for __ a bright -- er home.
  
  Bring my Harp to me a -- gain,
  Let me sing __ a gen -- tle strain—
  Let me hear __ its chords once more,
  Ere I pass to yon bright shore.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Soon I’ll be __ a -- mong the blest,
  Where the wea -- ry are at rest—
  Soon I’ll tread the gold -- en shore,
  Sing -- ing prais -- es ev -- er -- more.
  
  Now my boat is on the stream,
  I can see __ its wa -- ters gleam—
  Soon I’ll be __ where an -- gels roam—
  Dear old Harp, I’m go -- ing home.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 c |
  ees4~ ees4. c8 c c |
  
  f2. f8 f |
  f4~ f4. f8 f ees |
  g2. c,8 c |
  ees4~ ees4. c8 c c |
  f2. f8 des |
  
  c4~ c4. bes8 bes des |
  c2. ees8 ees |
  g4~ g4. ees8 ees16 ees8. |
  ees2. d8 d |
  d4~ d4. d8 d d |
  
  ees2. c8 c |
  ees4~ ees4. c8 c c  |
  f2. f8 aes |
  aes4~ aes4. g8 g g |
  aes2. \bar"||"
  
  %chorus
  aes8 aes |
  aes4~ aes4. aes8 aes8. ees16 |
  aes2. aes8 aes |
  aes4~ aes4. ees8 ees c |
  
  ees2. ees8 ees |
  ees4~ ees4. g8 f des |
  f2. f8 des |
  c4. g'8 g4 des8[ ees] |
  c2. \bar"|."
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
  aes8 aes |
  aes4~ aes4. aes8 aes aes |
  
  des2. des8 des |
  des4~ des4. des8 des des8 |
  ees2. aes,8 aes |
  aes4~ aes4. aes8 aes aes |
  des2. des8 aes |
  
  aes4~ aes4. g8 g g |
  ees2. g8 g |
  bes4~ bes4. g8 aes16 aes8. |
  g2. bes8 bes |
  bes4~ bes4. bes8 bes bes16[ aes] |
  
  g2. aes8 aes |
  aes4~ aes4. aes8 aes aes |
  des2. des8 des |
  c4~ c4. bes8 bes des |
  c2. \bar"||"

  %chorus
  c8 c |
  des4~ des4. des8 c8. aes16 |
  c2. c8 des |
  c4~ c4. aes8 g aes |
  
  g2. aes8 aes |
  aes4~ aes4. ees'8 des des |
  des2. aes8 aes |
  aes4. bes8 bes4 g8[ ees] |
  ees2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes8 ees  |
  aes,4~ aes4. aes8 aes aes |
  
  des2. des8 des |
  des4~ des4. des8 des des |
  ees2. aes8 ees |
  aes,4~ aes4. aes8 aes aes |
  des2. des8 des |
  
  ees4~ ees4. ees8 ees ees |
  aes,2. ees'8 ees |
  ees4~ ees4. ees8 aes,16 aes8. |
  ees'2. f8 f |
  bes,4~ bes4. bes8 bes bes |
  
  ees2. ees8 ees |
  aes,4~ aes4. aes8 aes aes |
  des2. des8 des |
  ees4~ ees4. ees8 ees ees |
  aes2. \bar"||"

  %chorus
  aes8 aes |
  aes4~ aes4. aes8 aes8. aes16 |
  aes2. aes8 aes |
  aes4~ aes4. aes8 ees f |
  
  ees2. aes,8 aes |
  aes4~ aes4. c8 des des |
  des2. des8 des |
  ees4. ees8 ees4\fermata ees |
  aes,2. \bar"|."
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

