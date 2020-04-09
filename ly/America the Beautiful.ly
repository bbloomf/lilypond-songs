\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"America the Beautiful"}}
  composer = \markup\oldStyleNum"Samuel Augustus Ward (1847–1903)"
  poet = \markup\oldStyleNum"Katherine Lee Bates (1859–1929)"
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #54
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
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  f4\mf |
  f4. d8 d4 f |
  f4. c8 c4 d |
  ees f g a |
  f2. \bar"" f4 |
  
  f4. d8 d4 f |
  f4. c8 c4 c' |
  b c d g, |
  c2. \bar"" f,4\f |
  
  d'4. d8 c4 bes |
  bes4. a8 a4 bes |
  c a g f |
  bes2. \bar"" bes4 |
  
  bes4. g8 g4 bes |
  bes4. f8 f4 f |
  g^\markup\italic"rall." bes f c' |
  bes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set associatedVoice = "altos"
  Oh beau -- ti -- ful for spa -- cious skies,
  For am -- ber waves of grain, __
  For pur -- ple moun -- tain maj -- es -- ties
  A -- bove the fruit -- ed plain! __
  A -- mer -- i -- ca! A -- mer -- i -- ca!
  God shed His grace on thee, __
  And crown thy good with bro -- ther -- hood
  From sea to shin -- ing sea!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set associatedVoice = "altos"
  O beau -- ti -- ful for pil -- grim feet,
  Whose stern im -- pas -- sion’d stress, __
  A thor -- ough -- fare for free -- dom beat
  A -- cross the wil -- der -- ness! __
  A -- mer -- i -- ca! A -- mer -- i -- ca!
  God mend thine ev -- ’ry flaw, __
  Con -- firm thy soul in self con -- trol,
  Thy lib -- er -- ty in law!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  f4 |
  d4. bes8 bes4 d |
  c4. a8 a4 a |
  c c ees ees |
  d2( ees4) c |
  
  d4. bes8 bes4 d |
  c4. c8 c4 f |
  f f e e |
  f( e ees) f |
  
  f4. f8 f4 d |
  ees4. f8 f4 f |
  f f ees ees |
  d2( ees4) f |
  
  ees4. ees8 ees4 ees |
  d4. d8 d4 f |
  g bes f ees |
  d2. \bar"|."
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
  \partial 4
  bes4 |
  bes4. f8 f4 bes |
  a4. f8 f4 f |
  a a c c |
  bes( gis a) a |
  
  bes4. f8 f4 bes |
  a4. a8 a4 a |
  a a bes bes |
  a( bes c)
  f, |
  
  bes4. bes8 bes4 bes |
  c4. c8 c4 bes |
  a a bes c |
  bes( f g) aes |
  
  g4. g8 g4 g |
  f4. bes8 bes4 f |
  g bes f a |
  bes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  bes,4 |
  bes4. bes8 bes4 f |
  c'4. c8 c4 f, |
  c' c f f, |
  bes( b c) f, |
  
  bes?4. bes8 bes4 g |
  c4. c8 c4 c |
  c c c g' |
  f( g a) f |
  
  f4. f8 d4 f |
  f4. c8 c4 d |
  ees f g a |
  bes( bes,2) bes4 |
  
  ees4. ees8 ees4 c |
  f4. f8 f4 f |
  g bes f f, |
  bes2. \bar"|."
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

