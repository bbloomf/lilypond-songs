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
  first-page-number = #95
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'16 a8. a a16 |
  c4. c8 |
  f,8 f g8. g16 |
  a4. a8 |
  d8. d16 d8 d |
  d c a f |
  g g a8. f16 |
  g4. \bar"" g8 |
  
  a8 a a8. a16 |
  c4. c8 |
  f,8. f16 g8. g16 |
  a4. a8 |
  d8. d16 d8. d16 |
  d8 c a f |
  g g a8. g16 |
  f4. \bar"||"
  
  %chorus
  c'8 |
  c4 d8. d16 |
  c4. c8 |
  d d f8. f16 |
  e4. e8 |
  f8. f16 c8. a16 |
  g8. a16 bes8. d16 |
  c8. c16 bes8. bes16 |
  a4. \bar"" c8 |
  
  c16 c8. d d16 |
  c4. c8 |
  d d f8. f16 |
  e4. e8 |
  f8. f16 c8. a16 |
  g8. a16 bes8. d16 |
  c8. c16 e8. e16 |
  f2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Broth -- ers of the plow,
  The pow -- er is with you;
  The world in ex -- pec -- ta -- tion waits
  For ac -- tion prompt and true,
  
  Op -- pres -- sion stalks a -- broad,
  Mo -- nop -- o -- lies a -- bound;
  Their gi -- ant hands al -- read -- y clutch
  The till -- ers of the ground.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Broth -- ers of the plow,
  In calm and qui -- et might,
  You’ve wait -- ed long and pa -- tient -- ly
  For what was yours by right;
  
  A fair re -- ward for toil,
  A free and o -- pen field;
  An hon -- est share for wife and home
  Of what your har -- vests yield.
  
  A -- wake, then, a -- wake!
  the great world must be fed,
  And heav -- en gives the pow -- er
  To the hand that holds the bread,
  
  Yes, broth -- ers of the plow,
  The peo -- ple must be fed,
  And heav -- en gives the pow -- er
  To the hand that holds the bread.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Broth -- ers of the plow,
  Come ral -- ly once a -- gain,
  Come gath -- er from the prai -- rie wide,
  The hill -- side and the plain;
  
  Not as in days of yore,
  With trump of bat -- tle’s sound,
  But come and make the world re -- spect
  The till -- ers of the ground.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f16 f8. f f16 |
  g4. g8 |
  d d e8. e16 |
  f4. f8 |
  f8. f16 f8 f |
  f f f f |
  e e f8. f16 |
  e4. e8 |
  
  f8 f8 f8. f16 |
  g4. g8 |
  d8. d16 e8. e16 |
  f4. f8 |
  f8. f16 f8. f16 |
  f8 f f f |
  e e e8. e16 |
  c4.
  
  
  %chorus
  f8 |
  f4 f8. f16 |
  f4. f8 |
  f f f8. f16 |
  g4. g8 |
  a8. a16 a8. f16 |
  f8. f16 f8. bes16 |
  a8. a16 g8. g16 |
  f4. f8
  
  f16 f8. f f16 |
  f4. f8 |
  f f f8. f16 |
  g4. g8 |
  a8. a16 a8. f16 |
  f8. f16 f8. bes16 |
  a8. a16 bes8. bes16 |
  a2 \bar"|."
  
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
  c16 c8. c c16 |
  c4. c8
  a a c8. c16
  c4. c8
  bes8. bes16 bes8 bes
  bes8 a a a 
  c c c8. a16
  c4. c8
  
  c8 c8 c8. c16 |
  c4. c8
  a8. a16 c8. c16
  c4. c8
  bes8. bes16 bes8. bes16
  bes8 a a a 
  bes bes c8. bes16
  a4.
  \bar"||"
  
  %chorus
  a8 |
  a4 bes8. bes16 |
  a4. a8 |
  a a b8. b16 |
  c4. c8 |
  c8. c16 c8. c16 |
  d8. d16 d8. f16 |
  f8. c16 c8. c16 |
  c4. a8 |
  
  a16 a8. bes bes16 |
  a4. a8 |
  a a b8. b16 |
  c4. c8 |
  c8. c16 c8. c16 |
  d8. d16 d8. f16 |
  f8. c16 c8. c16 |
  c2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,16 f8. f f16 |
  e4. e8 |
  d8 d c8. c16 f4. f8 |
  bes,8. bes16 bes8 bes |
  bes8 f' f f |
  c c f8. f16 |
  c4. \bar""
  
  c8
  f8 f8 f8. f16 |
  e4. e8 |
  d8. d16 c8. c16 f4. f8 |
  bes,8. bes16 bes8. bes16 |
  bes8 f' f f |
  c c c8. c16 |
  f4. \bar"||"
  
  %chorus
  f8 |
  f4 f8. f16 |
  f4. f8 |
  d d d8. d16 |
  c4. c8 |
  f8. f16 f8. f16 |
  bes,8. bes16 bes8. bes16 |
  c8. c16 c8. c16 |
  f4. f8 |
  
  f16 f8. f f16 |
  f4. f8 |
  d d d8. d16 |
  c4. c8 |
  f8. f16 f8. f16 |
  bes,8. bes16 bes8. bes16 |
  c8. c16 c8. c16 |
  f2 \bar"|."
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Hand that Holds the Bread"}}
  composer = \markup\oldStyleNum"George Frederick Root (1820–1895)"
  tagline = ""
}}


