\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Hazel Dell"}}
  composer = \markup\oldStyleNum"George Frederick Root (1820–1895)"
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
  first-page-number = #61
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
}

sopMusic = \relative c' {
	\partial 4
  b'8 c |
  d4 g, g8 a g fis |
  e2 g |
  d8 << {g4.} {s8 \teeny g4} >> \normalsize g4 b |
  a2. \bar"" b8 c |
  
  d4 g, g8 a g fis |
  e2 g |
  d8 << d'4. {s8 \teeny d4} >> \normalsize b a |
  g2. \bar"" b8 c |
  
  d4 g, g8 a g fis |
  e2 g |
  d4 g g b |
  a2. \bar"" b8 c |
  
  
  d4 g, g8 a g fis |
  e2 g |
  d8 d'4. b4 a |
  g2. b4\rest \bar"||"
  
  %Chorus
  c4. c8 b4 b |
  b a g a |
  b d, e g |
  a2. \bar"" b8 c |
  
  d4 g, g8 a g fis |
  e2 g |
  d8 d'4. b4 a |
  g2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	In the Ha -- zel Dell my Nel -- ly’s sleep -- ing,
  Nel -- ly "" loved so long!
  And my lone -- ly lone -- ly watch I’m keep -- ing,
  Nel -- ly "" lost and gone;
  Here in moon -- light oft -- en we have wan -- dered
  Through the si -- lent shade,
  Now where leaf -- y branch -- es droop -- ing down -- ward,
  Lit -- tle Nel -- ly’s laid.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  In the Ha -- zel Dell my Nel -- ly’s sleep -- ing,
  Where "" the flow -- ers wave,
  And the si -- lent stars are night -- ly weep -- ing,
  O’er "" poor Nel -- ly’s grave;
  Hopes that once my bos -- om fond -- ly cher -- ished
  Smile no more on me,
  Ev -- ’ry dream of joy a -- las has per -- ished,
  Nel -- ly dear, with thee.
  
  All a -- lone my watch I’m keep -- ing
  In the Ha -- zel Dell,
  For my dar -- ling Nel -- ly’s near me sleep -- ing,
  Nel -- ly dear, fare -- well.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Now I’m wea -- ry, friend -- less, and for -- sak -- en,
  Watch -- \skip1 ing here a -- lone,
  Nel -- ly, thou no more will fond -- ly cheer me,
  With "" thy lov -- ing tone;
  Yet for -- ev -- er shall thy gen -- tle im -- age
  In my mem -- ’ry dwell.
  And my tears thy lone -- ly grave shall moist -- en,
  Nel -- ly dear, fare -- well.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 e |
  d4 d d8 d d d |
  c2 e |
  d8 << d4. {s8 \teeny d4} >> \normalsize d d |
  d2. g8 e |
  
  d4 d d8 d d d |
  c2 e |
  d8 << g4. {s8 \teeny g4} >> \normalsize g fis |
  g2. g8 e |
  
  d4 d d8 d d d |
  c2 e |
  d4 d d d |
  d2. g8 e |
  
  d4 d d8 d d d |
  c2 e |
  d8 g4. g4 fis |
  g2. s4 \bar"||"
  
  %Chorus
  g4. g8 g4 g |
  g d d d |
  d b c d |
  fis2. g8 e |
  
  d4 d d8 d d d |
  c2 e |
  d8 g4. g4 fis |
  g2. \bar"|."
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
  g8 g |
  g4 g b8 c b a |
  g2 c |
  b8 << b4. {s8 \teeny b4} >> \normalsize b g |
  fis2. g8 g |
  
  g4 g b8 c b a |
  g2 c |
  b8 << b4. {s8 \teeny b4} >> \normalsize d c |
  b2. g8 g |
  
  g4 g b8 c b a |
  g2 c |
  b4 b b g |
  fis2. g8 g |
  
  g4 g b8 c b a |
  g2 c |
  b8 b4. d4 c |
  b2. s4 \bar"||"
  
  %Chorus
  e4. e8 d4 d |
  d c b c |
  d g, c b |
  a2. g8 g |
  g4 g b8 c b a |
  g2 c |
  b8 b4. d4 c |
  b2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,8 c |
  b4 b g8 g g g |
  c2 c |
  d8 << b4. {s8 \teeny b4} >> \normalsize g4 g |
  d'2. d8 c |
  
  b4 b g8 g g g |
  c2 c |
  d8 << d4. {s8 \teeny d4} >> \normalsize d4 d |
  g,2. d'8 c |
  
  b4 b g8 g g g |
  c2 c |
  d4 b g g |
  d'2. d8 c |
  
  b4 b g8 g g g |
  c2 c |
  d8 d4. d4 d |
  g,2. d'4\rest \bar"||"
  
  %Chorus
  g4. g8 g4 g |
  d d d d |
  g, g g g |
  d'2. d8 c |
  
  b4 b g8 g g g |
  c2 c |
  d8 d4. d4 d |
  g,2. \bar"|."
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
    \tempo 4 = 110
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


