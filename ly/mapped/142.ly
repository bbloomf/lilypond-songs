﻿\version "2.14.2"
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
       (padding . 0)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #142
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
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  f8 |
  bes4 bes8 a[ g] a |
  bes4 f8 f4 f8 |
  g4 g8 bes[ a] g |
  f4. b4\rest \bar"" f8 |
  a4 a8 bes4 bes8 |
  
  c4 c8 d4\fermata bes8 |
  g4 g8 c8[ c] bes |
  bes4.( a4) f8 |
  bes4 bes8 a8[ g] a |
  bes4 f8 f4 f8 |
  
  g4 g8 bes[ a] g |
  f4. b4\rest f8 |
  a4 a8 bes4 bes8 |
  c4 f8 d4\fermata \bar"" bes8 |
  g[ c] bes a[ g] a |
  
  bes4. d |
  g,8[ c] bes a[ g] a |
  bes4. d |
  ees4 c8 f4 a,8 |
  bes4. bes4\rest f8 |
  
  f[ c'] a f4 f8 |
  f[ d'] bes f4 d'8 |
  ees4 c8 d4 bes8 |
  c4. b4\rest \bar"" d8 |
  d4 d8 d4\fermata c8 |
  
  c4 bes8 g4 bes8 |
  d4 c8 bes4 a8 |
  g4.\fermata f\fermata |
  bes4 bes8 a[ g] a |
  bes4 f8 f4 f8 |
  
  g4 g8 bes[ a] g |
  f4. b4\rest f8 |
  a4 a8 bes4 bes8 |
  c4 f8 d4\fermata bes8 |
  g[ c] bes a[ g] a |
  
  bes4. d |
  g,8[ c] bes a[ g] a |
  bes4. d |
  ees4 c8 f4\fermata a,8 |
  bes4. bes4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	A man whose name was John -- ny Sands,
  Had mar -- ried Bet -- ty Hague,
  And though she brought him gold and lands,
  \set ignoreMelismata = ##t
  She proved a ter -- ri -- ble plague; _
  \unset ignoreMelismata
  For, oh, she was a scold -- ing wife,
  Full of ca -- price and whim,
  He said that he was tired of life,
  And she was tired of him,
  And she was tired of him,
  And she was tired of him;
  
  Says he, “Then I will drown my -- self,
  The riv -- er runs be -- low;”
  Says she, “Pray do, you sil -- ly elf,
  I wished it long a -- go.”
  Says he, “Up -- on the brink I’ll stand,
  Do you run down the hill,
  And push me in with all your might.”
  Says she, “My love, I will,”
  Says she, “My love, I will,”
  Says she, “My love, I will.”
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  “For fear that I should cour -- age lack,
  And try to save my life,
  Pray, tie my hands be -- hind my back,”
  “I will,” re -- plied his wife, __
  She tied them fast, as you may think,
  And when se -- cure -- ly done,
  “Now stand,” she says, “up -- on the brink,
  And I’ll pre -- pare to run,
  And I’ll pre -- pare to run,
  And I’ll pre -- pare to run.”
  
  All down the hill his lov -- ing bride
  Now ran with all her force,
  To push him in, he stepped a -- side,
  And she fell in, of course;
  Now splash -- ing, dash -- ing, like a fish,
  “Oh, save me, John -- ny Sands.”
  “I can’t, my dear, though much I wish,
  For you have tied my hands,
  For you have tied my hands,
  For you have tied my hands.”
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
  f8 |
  d4 d8 ees4 ees8 |
  d4 d8 d4 d8 |
  ees4 ees8 g[ f] ees |
  d4. s4 d8 |
  ees4 ees8 d4 d8 |
  
  f4 f8 f4 f8 |
  g4 g8 g[ g] g8 |
  f4.~ f4 ees8 |
  d4 d8 ees4 ees8 |
  d4 d8 d4 d8 |
  
  ees4 ees8 g[ f] ees |
  d4. s4 d8 |
  ees4 ees8 d4 d8 |
  f4 a8 bes4 f8 |
  ees4 g8 f4 ees8 |
  
  d4. f |
  ees4 g8 f4 ees8 |
  d4. f |
  g4 g8 a4 ees8 |
  d4. s4 \bar"" f8 |
  
  f4 f8 ees4 ees8 |
  d[ f] d d4 f8 |
  g4 g8 f4 d8 |
  f4. s4 f8 |
  fis4 fis8 fis4 fis8 |
  
  g4 d8 d4 g8 |
  fis4 fis8 fis4 fis8 |
  d4. ees |
  d4 d8 ees4 ees8 |
  d4 d8 d4 d8 |
  
  ees4 ees8 g[ f] ees |
  d4. s4 d8 |
  ees4 ees8 d4 d8 |
  f4 a8 bes4 f8 |
  g4 g8 f4 ees8 |
  
  d4. f |
  ees4 g8 f4 ees8 |
  d4. f |
  g4 g8 a4 ees8 |
  d4. s4 \bar"|."
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
  f,8 |
  bes4 bes8 c4 c8 |
  bes4 bes8 bes4 bes8 |
  bes4 bes8 bes4 bes8 |
  bes4. s4 bes8 |
  c4 c8 bes4 bes8 |
  
  a4 a8 bes4 bes8 |
  c4 c8 c[ c] c8 |
  d4.( c4) a8 |
  bes4 bes8 c4 c8 |
  bes4 bes8 bes4 bes8 |
  
  bes4 bes8 bes4 bes8 |
  bes4. s4 bes8 |
  c4 c8 bes4 bes8 |
  a4 c8 bes4 bes8 |
  bes4 c8 c4 c8 |
  
  bes4. bes |
  bes4 c8 c4 c8 |
  bes4. bes |
  bes4 c8 c4 c8 |
  bes4. s4 f8 |
  
  a4 c8 a4 a8 |
  bes4 bes8 bes4 bes8 |
  bes4 bes8 bes4 bes8 |
  a4. s4 bes8 |
  a4 a8 a4 a8 |
  
  bes4 bes8 bes4 bes8 |
  a4 a8 a4 a8 |
  bes4. c |
  bes4 bes8 c4 c8 |
  bes4 bes8 bes4 bes8 |
  
  bes4 bes8 bes4 bes8 |
  bes4. s4 bes8 |
  c4 c8 bes4 bes8 |
  a4 c8 bes4 bes8 |
  bes4 c8 c4 c8 |
  
  bes4. bes |
  bes4 c8 c4 c8 |
  bes4. bes |
  bes4 c8 c4 c8 |
  bes4. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8 |
  bes,4 bes8 f'4 f8 |
  bes,4 bes8 bes4 bes8 |
  ees4 ees8 ees4 ees8 |
  bes4. d4\rest bes8 |
  f'4 f8 f4 f8 |
  
  f4 f8 bes,4\fermata d8 |
  ees4 ees8 e[ e] e8 |
  f4.~ f4 f8 |
  bes,4 bes8 f'4 f8 |
  bes,4 bes8 bes4 bes8 |
  
  ees4 ees8 ees4 ees8 |
  bes4. d4\rest bes8 |
  f'4 f8 f4 f8 |
  f4 f8 bes,4 d8 |
  ees4 ees8 f4 f8 |
  
  bes,4. bes' |
  ees,4 ees8 f4 f8 |
  bes,4. bes |
  ees4 ees8 f4 f8 |
  bes,4. d4\rest f8 |
  
  f4 f8 f4 f8 |
  bes,4 bes8 bes4 bes8 |
  ees4 ees8 f4 f8 |
  f4. d4\rest bes8 |
  d4 d8 d4 d8 |
  
  g4 g8 g4 g,8 |
  d'4 d8 d4 d8 |
  g4.\fermata f\fermata |
  bes,4 bes8 f'4 f8 |
  bes,4 bes8 bes4 bes8 |
  
  ees4 ees8 ees4 ees8 |
  bes4. d4\rest bes8 |
  f'4 f8 f4 f8 |
  f4 f8 bes,4 d8 |
  ees4 ees8 f4 f8 |
  
  bes,4. bes' |
  ees,4 ees8 f4 f8 |
  bes4. bes, |
  ees4 ees8 f4\fermata f8 |
  bes,4. d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Johnny Sands"}}
  composer = \markup\oldStyleNum"John Sinclair, 1842"
  tagline = ""
}}


global = {
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	c4 c c c |
  f f f f |
  g a bes c |
  f,2 b4\rest f4
}
sopWords = \lyricmode {
  He that will an ale -- house keep,
  Must have three things in store,
  A
}

sopWordsII = \lyricmode {
  cham -- ber and a fea -- ther bed;
  A chim -- ney and a Hey non -- ny non -- ny
}

sopWordsIII = \lyricmode {
  Hey non -- ny non -- ny
  Hey non -- ny no,
  Hey non -- ny no,
  Hey non -- ny no.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c' c c c |
  d c a f |
  bes c d e |
  f e8 d c4 a |
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
  c'4 bes8 a g4 e |
  a a8 g f4 f' |
  e8 d c4 bes8[ d] c bes |
  a2 b\rest \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Staff = women <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "altosIII"  \lyricsto "tenors" \sopWordsIII
    
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"He that will an alehouse keep"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  tagline = ""
}}


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
	aes'2. c4 |
  bes g ees2 |
  f2. aes4 |
  g ees c2 \bar"||"
}
sopWords = \lyricmode {
  Round and round we go
  While the north winds blow.
}

sopWordsII = \lyricmode {
  Swift -- ly as the swal -- lows go,
  A -- cross the spark -- ling ice we fly.
}

sopWordsIII = \lyricmode {
  Round and round and to and fro
  While loud the cold wind whis -- tles by.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  aes'4 ees' ees aes, |
  g ees' ees g, |
  f des' des f, |
  ees c' aes2 \bar"||"
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
  ees'4 c aes ees' |
  ees bes g ees' |
  des aes f des' |
  c aes ees2 \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Staff = women <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "altosIII"  \lyricsto "tenors" \sopWordsIII
    
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Skating"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  tagline = ""
}}


