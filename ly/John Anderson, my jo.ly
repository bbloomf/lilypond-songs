\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"John Anderson, my jo"}}
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum"Harmonized by Max Vogrich (1852–1916)"
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
  first-page-number = #32
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c'' {
	\partial 4 g8[ f] |
  d4 g g a |
  bes2 bes4 c8[ bes] |
  a4. g8 f4 e |
  f2 bes4\rest g8[ f] |
  
  d4 g g a |
  bes2 bes4 c |
  d4. c8 bes4 c |
  d2 bes4\rest f'4 |
  d4. c8 bes4 d |
  
  f2 d4 d |
  c4. bes8 a4 bes |
  c2 bes4\rest bes8[ c] |
  d4 bes c a |
  bes g d'\fermata g,8[ f] |
  
  d4 g g f |
  g2. \bar "||"
  
	g8[ f] |
  d4 g g a |
  bes2 bes4 c8[ bes] |
  a4. g8 f4 e |
  f8 f4. bes4\rest g8[ f] |
  
  d4 g g a |
  bes2 bes4 c |
  d4. c8 bes4 c |
  d8 d4. bes4\rest f'4 |
  d4. c8 bes4 d |
  
  f2 d4 d |
  c4. bes8 a4 bes |
  c2 bes4\rest bes8 c |
  d4 bes c a |
  bes g d'\fermata g,8[^\markup\italic"più adagio" f] |
  
  d4 g g f |
  g2. \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	John An -- der -- son, my jo, John,
  When we were first ac -- quent,
  Your locks were like the ra -- ven,
  Your bon -- nie brow was brent;
  But now your brow is bald, John,
  Your locks are like the snow,
  Yet, bless -- ings on your frost -- y pow,
  John An -- der -- son, my jo.
  
  \set stanza = #"2. "
	John An -- der -- son, my jo, John,
  We clamb the hill to -- gith -- er;
  And mon -- ie~a cant -- y day, John,
  We’ve had wi’ ane an -- ith -- er.
  Now we maun tot -- ter down, John,
  But hand in hand we’ll go,
  And we’ll sleep to -- gith -- er at the foot,
  John An -- der -- son, my jo.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

altoMusic = \relative c' {
  \partial 4 g'8[ f] |
  d4 g g f |
  f2 f4 g |
  f4. e8 c4 c |
  c2 s4 c4 |
  
  d4 d d f |
  f2 f4 a |
  bes4. a8 f4 a |
  bes2 s4 d |
  bes4. f8 d4 bes' |
  
  d2 bes4 bes |
  a4. f8 f4 f |
  f2 s4 f8[ a] |
  bes4 f f f |
  d g fis d8[ c] |
  
  d4 d d d |
  d2. \bar "||"
  
  g8[ f] |
  d4 g g f |
  f2 f4 g |
  f4. e8 c4 c |
  c8 c4. s4 c4 |
  
  d4 d d f |
  f2 f4 a |
  bes4. a8 f4 a |
  bes8 bes4. s4 d |
  bes4. f8 d4 bes' |
  
  d2 bes4 bes |
  a4. f8 f4 f |
  f2 s4 f8 a |
  bes4 f f f |
  d g fis d8[ c] |
  
  d4 d d d |
  d2. \bar "|."
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
  \partial 4 g8[ f] |
  d4 g g c |
  d2 d4 d |
  c4. c8 a4 g |
  a2 s4 c |
  
  bes4 bes bes c |
  d2 f4 f |
  f4. f8 f4 f |
  f2 s2 |
  s1 |
  
  s2. f4 |
  f4. d8 c4 d |
  a2 s4 d8[ c] |
  bes4 bes a c |
  bes d a bes8[ c] |
  
  d4 bes g a |
  bes2. \bar "||"
  
  g8[ f] |
  d4 g g c |
  d2 d4 d |
  c4. c8 a4 g |
  a8 a4. s4 c |
  
  bes4 bes bes c |
  d2 f4 f |
  f4. f8 f4 f |
  f8 f4. s2 |
  s1 |
  
  s2. f4 |
  f4. d8 c4 d |
  a2 s4 d8 c |
  bes4 bes a c |
  bes d a bes8[ c] |
  
  d4 bes g a |
  bes2. \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4 g8[ f] |
  d4 g g f |
  bes,2 bes'4 g |
  c,4. c8 c4 c |
  f2 d4\rest a4 |
  
  bes d g f |
  bes,2 d4 f |
  bes4. c8 d4 c |
  bes2 d,2\rest |
  d1\rest |
  
  d2\rest d4\rest bes'4 |
  f4. f8 f4 f |
  f2 d4\rest f |
  bes,4 d f f |
  g bes d,_\fermata bes8[ a] |
  
  bes4 g bes d |
  g,2. \bar "||"
  
  g'8[ f] |
  d4 g g f |
  bes,2 bes'4 g |
  c,4. c8 c4 c |
  f8 f4. d4\rest a4 |
  
  bes d g f |
  bes,2 d4 f |
  bes4. c8 d4 c |
  bes8 bes4. d,2\rest |
  d1\rest |
  
  d2\rest d4\rest bes'4 |
  f4. f8 f4 f |
  f2 d4\rest f8 f |
  bes,4 d f f |
  g bes d,_\fermata bes8[ a] |
  
  bes4 g bes d |
  g,2. \bar "|."
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
    \new Lyrics = "altos"  \lyricsto "altos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
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
    \tempo 4 = 130
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


