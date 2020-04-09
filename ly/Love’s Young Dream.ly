\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Love’s Young Dream"}}
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
  composer = \markup\oldStyleNum"Irish Air"
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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
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
	\partial 4.
  d'4 c8 |
  b4 g8 a4 e8 |
  g4 e8 d4 g8 |
  a4. a |
  a d4 c8 |
  b4 g8 a4 e8 |
  g4 e8 d4 fis8 |
  
  g4. g |
  g4. g4( a8) |
  b4 c8 d4 d8 |
  e4 fis8 g4 e8 |
  d4 b8 a4 g8 |
  a4. d4 c8 |
  
  b8 g4 a4 e8 |
  g4 e8 d4 g8 |
  a4. a |
  a d4 c8 |
  b8 g4 a4 e8 |
  g4 e8 d4 e8 |
  g4. g |
  g4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Oh! the days are gone, when beau -- ty bright
  My heart’s chain wove;
  When my dream of life from morn till night,
  Was love, still love;
  New hope may bloom, and days may come
  Of mild -- er, calm -- er beam,
  But there’s noth -- ing half so sweet in life
  As love’s young dream,
  No! there’s noth -- ing half so sweet in life
  As love’s young dream.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Though the bard to pur -- er fame may soar,
  When wild youth’s past;
  Though he win the wise, who frowned be -- fore,
  To smile at last;
  He’ll nev -- er meet a joy so sweet
  In all his noon of fame,
  As when first he sung to wom -- an’s ear
  His soul -- felt flame,
  And at ev -- ’ry close she blushed to hear
  The one loved name.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  No! that hal -- low’d form is ne’er for -- got,
  Which first love traced;
  Still it ling -- ’ring haunts the green -- est spot
  On mem -- ’ry’s waste;
  ’Twas o -- dor fled, as soon as shed;
  ’Twas morn -- ning’s wing -- ed dream;
  ’Twas a light that ne’er can shine a -- gain
  On life’s dull stream,
  Oh! ’twas light which ne’er can shine a -- gain
  On life’s dull stream.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4.
  b'4 a8 |
  g4 d8 e4 c8 |
  b4 c8 b4 g'8 |
  g4. g |
  fis g4 fis8 |
  g4 d8 e4 c8 |
  b4 c8 b4 c8 |
  
  d4. e8[ d c] |
  d4. b4( d8) |
  g4 a8 b4 b8 |
  c4 a8 b4 c8 |
  b4 g8 e4 e8 |
  g4( fis8) fis4 fis8 |
  
  g8 e4 e4 c8 |
  b4 c8 b4 d8 |
  g4. g |
  fis g4 fis8 |
  g8 e4 e4 c8 |
  b4 c8 b4 b8 |
  b4. c |
  d \bar"|."
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
  \partial 4.
  d4 d8 |
  d4 b8 c4 g8 |
  g4 g8 g4 b8 |
  e4. e |
  d d4 d8 |
  d4 b8 c4 g8 |
  g4 g8 g4 a8 |
  
  b4. c |
  b4. g |
  g4 g8 g4 g8 |
  g4 d'8 d4 g,8 |
  g4 g8 c4 cis8 |
  d4. d4 d8 |
  
  d8 b4 c4 g8 |
  g4 g8 g4 g8 |
  e'4. e |
  d d4 d8 |
  d8 b4 c4 g8 |
  g4 g8 g4 g8 |
  g4. c |
  b4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4.
  g4 g8 |
  g4 g8 g4 g8 |
  g,4 g8 g4 d'8 |
  c4. cis |
  d4( c?8) b4 a8 |
  g4 g8 g4 g8 |
  g4 g8 g4 d'8 |
  
  d4. c4( e8) |
  g4. g |
  g4 g8 g4 g8 |
  g4 g8 g4 g8 |
  g4 g8 a4 a8 |
  d,4. d4 d8 |
  
  g8 g4 g4 g8 |
  g,4 g8 g4 b8 |
  c4. cis |
  d4( c?8) b4 a8 |
  g8 g4 g4 g8 |
  g4 g8 g4 e'8 |
  d4. e |
  g \bar"|."
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
    \tempo 4 = 120
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


