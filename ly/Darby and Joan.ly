\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Darby and Joan"}}
  composer = \markup\oldStyleNum"James Lynam Molloy (1837–1909)"
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
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
  first-page-number = #71
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
  \key f \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
}

sopMusic = \relative c' {
	f4 g8 a8. g16 f8 |
  g4 f8 f4. |
  g4 a8 bes8. a16 g8 |
  a4 g8 g4. |
  g8.\cresc a16\! bes8 c4 a8 |
  
  g4 f8 f8. e16 d8 |
  e4. d |
  c2. |
  c4 d8 e8. f16 g8 |
  a4 e8 e4. |
  fis4 << e8 {s16 \teeny e16*1/2 d \normalsize} >> d8. fis16 a8 |
  
  b4 g8 g4. |
  c4. b8. g16 e8 |
  a4 a8 g4. |
  g8.(^\markup\italic"rall." f16) e8 g f e |
  d4 c8 c4.\fermata |
  f8.^\markup{\dynamic"p" \italic" meno mosso"} g16 a8 bes4. |
  
  a8. g16 f8 g4. |
  g8. a16 bes8 c8. a16 g8 |
  f4 d8 c4. |
  c8 d f g a f |
  g4 f8 f4.\fermata \bar"|."
}
sopWords = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
	Dar -- by dear, we are old and gray,
  Fif -- ty years since our wed -- ding day,
  Shad -- ow and sun for ev -- ’ry one as the years roll on:
  Dar -- by dear, when the world went wry,
  Hard and "" "" sor -- row -- ful then was I,
  Ah! lad, how you cheered me then,
  “Things will be bet -- ter, sweet wife, a -- gain!”
}
chorusWords = \lyricmode {
  \set ignoreMelismata = ##t
  \repeat unfold 61 \skip 1
  Al -- ways the same, Dar -- by my own,
  Al -- ways the same to your old wife Joan,
  Al -- ways the same to your old wife Joan.
}

sopWordsII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"2. "
  Dar -- by dear, but my heart was wild
  When we bur -- ied our ba -- by child,
  Un -- til you whis -- pered, “Heav’n knows best!” and my heart found rest;
  Dar -- by dear, ’twas your lov -- ing hand
  Show’d "" me the way to the bet -- ter land;
  Ah! lad, as you kissed each tear,
  Life __ _ grew bet -- ter and Heav’n more near:
  
  
  Al -- ways the same, Dar -- by my own,
  Al -- ways the same to your old wife Joan,
  Al -- ways the same to your old wife Joan.
}

sopWordsIII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"3. "
  Hand in hand when our life was May,
  Hand in hand when our hair is gray,
  Shad -- ow and sun for ev -- ’ry -- one as the years roll on:
  Hand in hand when the long night -- tide
  Gent -- ly "" "" cov -- ers us side by side:
  Ah! lad, tho’ we know not when,
  Love will be with us for -- ev -- er then:
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c4 e8 f8. e16 f8 |
  d4 d8 d4. |
  d4 d8 d8. d16 d8 |
  e4 e8 e4. |
  e8. e16 e8 f4 f8 |
  
  d4 d8 d8. c16 d8 |
  c4. b |
  c2. |
  c4 b8 bes8. a16 e'8 |
  e4 cis8 cis4. |
  c?4 << c8 {s16 \teeny c16*1/2 c \normalsize} >> c8. c16 d8 |
  
  d4 d8 d4. |
  e4. e8. e16 e8 |
  f4 f8 e4. |
  e8.( d16) cis8 d d c |
  b4 c8 c4. |
  c8. e16 f8 f4. |
  
  f8. e16 d8 e4. |
  e8. f16 e8 f8. f16 e8 |
  d4 b8 c4. |
  a8 bes? c e e c |
  c4 c8 c4. \bar"|."
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
  a4 bes8 c8. bes16 a8 |
  bes4 a8 a4. |
  bes4 c8 d8. c16 bes8 |
  c4 c8 c4. |
  c8. c16 c8 a4 c8 |
  
  bes4 a8 a8. a16 a8 |
  g4. f |
  e2. |
  e4 f8 g8. f16 g8 |
  a4 a8 a4. |
  a4 << g8 {s16 \teeny g16*1/2 fis} >> \normalsize fis!8. a16 fis8 |
  
  g4 b8 b4. |
  a4. g8. b16 g8 |
  c4 c8 c4. |
  a8.( a16) a8 a a g |
  g4 e8 e4. |
  a8. bes16 c8 d4. |
  
  c8. bes16 a8 c4. |
  c8. c16 c8 a8. c16 bes8 |
  a4 f8 e4. |
  f8 f a bes c a |
  bes4 a8 a4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 f8 f8. f16 f8 |
  d4 d8 d4. |
  g4 g8 g8. g16 g8 |
  c,4 c8 c4. |
  bes'8. a16 g8 f4 f8 |
  
  d4 d8 d8. e16 f8 |
  g4. g, |
  c2. |
  c4 c8 c8. c16 c8 |
  cis4 a8 a4. |
  d4 << d8 {s16 \teeny d16*1/2 d} >> \normalsize d8. d16 d8 |
  
  g,4 g'8 g4. |
  a,4. e'8. e16 e8 |
  f4 f8 c4. |
  cis8.( a16) a8 d d e |
  g4 c,8 c4.\fermata |
  f8. f16 f8 f4. |
  
  f8. f16 f8 c4. |
  bes'8. a16 g8 f8. f16 c8 |
  d4 g,8 c4. |
  c8 c c c c c |
  c4 c8 f4.\fermata \bar"|."
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
    \tempo 4 = 80
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


