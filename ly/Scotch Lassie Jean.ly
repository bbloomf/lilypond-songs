\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Scotch Lassie Jean"}}
  composer = \markup\oldStyleNum"Harry Miller, 1873"
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
       (stretchability . 50))
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
  first-page-number = #62
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
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	\partial 4
  ees4 |
  
  c'4 bes8 aes c8~ c bes aes |
  bes~ bes aes f aes4 aes8[ bes] |
  c4~ c8 aes g( aes) des c |
  bes2 b4\rest ees, |
  
  c'8 c bes aes c4 bes8[ aes] |
  bes aes aes f aes4 aes8 bes |
  c c bes aes ees ees g g |
  
  aes2 b4\rest ees,8 ees |
  des' des des des des~ des ees des |
  des c c c c4 \teeny c8 \normalsize c |
  
  %page2/43 soprano
  bes8~ bes~ bes bes d~ d c d |
  ees2. ees,4 |
  c' bes8 aes c4 bes8 aes |
  
  bes aes aes f aes4 aes8 bes |
  c c bes aes ees~ ees g g
  \partial 2. aes2. \bar"||"\break
  
  %Chorus
  \slurSolid
  c2 des4. des8 |
  ees4 ees c2 |
  bes4 bes8 bes d d c d |
  ees4( d des)\fermata ees, |
  
  c'8 c bes aes c4 bes8 aes |
  bes aes aes f aes aes aes bes |
  c c bes aes ees4 g8 g |
  aes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  In Scot -- land’s fair lands __ _ o -- ver moun -- _ tains and rills,
  That’s _ where __ _ I roamed for ma -- ny~a day
  In look -- ing at the lads and _ las -- sies on the green,
  In the fair old land of Scot -- land far a -- way.
  I have wait -- ed for her com -- ing but she has not come as yet,
  "" The truth _ seems to dawn up -- on me plain;
  They say she is false, but I still be -- lieve her true,
  She’s my dar -- ling blue -- eyed Scotch _ las -- sie, Jean.
  
  \set stanza = \markup\dynamic"f  "
  Oh, Jean, my bon -- nie Jean, come to your lad -- die once a -- gain! __ _ _
  They
  \set stanza = \markup\dynamic"p  "
  say that you are false, but I still be -- lieve you mine,
  You are my bon -- nie \once \override LyricHyphen #'minimum-distance = #0.7 blue -- eyed Scotch las -- sie, Jean.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  She said she would meet me, but I’ve wait -- ed long in vain,
  In __ _ lands far a -- way __ _ she does roam;
  Her prom -- ise she will keep, oh! _ break it not, my Jean!
  We’ll be hap -- py in our bon -- nie lit -- tle home.
  O then let me not long wait, __ _ let me meet thee soon, my Jean,
  And the heav -- ens __ _ will smile __ _ on our love;
  And when life is dead we will leave this earth -- ly scene,
  And our hearts will dwell in joy and bliss a -- bove.
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
  c4 |
  ees4 ees8 ees ees~ ees ees ees |
  f~ f f des ees4 ees |
  ees4~ ees8 ees ees~ ees f ees |
  ees2 s4 c4 |
  
  ees8 ees ees ees ees4 ees |
  f8 f f des ees4 ees8 f |
  ees ees ees ees des des des des |
  c2 s4 c8 c |
  
  g' g g g g~ g g g |
  aes aes aes aes aes4 \teeny aes8 \normalsize aes |
  
  
  
  g8~ g( ees) g f~ f f f |
  \slurSolid ees4( d des) ees |
  \slurDashed ees4 ees8 ees e4 f8 f |
  f f f f ees4 ees8 f |
  ees8 ees ees ees c8~ c des des c2. \bar"||"
  
  
  \slurSolid
  aes'2 g4. g8 |
  aes4 aes aes2 |
  g4 ees8 g f f f f |
  ees4( f g) ees |
  
  ees8 ees ees ees e4 f8 f |
  f f f f ees ees ees f |
  ees ees ees ees c4 des8 des |
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
  aes4 |
  aes4 g8 aes aes~ aes g aes |
  des~ des aes aes c4 c |
  aes4~ aes8 aes8 bes( aes) aes aes |
  g2 s4 aes4 |
  
  aes8 aes g aes aes4 g8[ aes] |
  des8 des aes aes c4 aes8 aes |
  aes aes des c g g bes bes |
  aes2 s4 aes8 aes |
  
  bes bes bes bes bes~ bes bes bes |
  ees ees ees ees f4 \teeny f8 \normalsize f |
  
  
  ees8~ ees( bes) bes aes~ aes aes aes |
  \slurSolid g4( aes bes) des |
  \slurDashed c4 des8 c c4 c8 c |
  des8 des des aes aes4 aes8 aes |
  aes aes des c aes~ aes bes bes
  aes2. \bar"||"
  
  
  \slurSolid
  ees'2 ees4. ees8 |
  ees4 ees ees2 |
  ees4 bes8 bes aes aes aes aes |
  g4( aes bes)\fermata des |
  
  c8 c des c c4 c8 c |
  des des des aes aes aes aes aes |
  aes aes des c aes4 bes8 bes |
  aes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,4 |
  aes bes8 c aes~ aes bes c |
  des~ des des des aes4 aes |
  aes4~ aes8 c ees( c) des ees |
  ees2 d4\rest aes4 |
  
  aes8 aes bes8 c aes4 bes8[ c] |
  des des des des aes4 c8 des |
  ees ees ees ees ees ees ees ees |
  aes,2 d4\rest aes8 aes |
  
  ees' ees ees ees ees~ ees ees ees |
  aes, aes c ees f4 \teeny f8 \normalsize f |
  
  
  bes8~ bes( g) ees bes~ bes bes bes |
  \slurSolid ees4( f g) ees4 |
  aes4 aes8 aes g4 f8 f |
  des des des des c4 c8 des |
  ees ees ees ees ees~ ees ees8 ees
  aes2. \bar"||"
  
  
  \slurSolid
  aes2 bes4. bes8 |
  c4 c aes2 |
  bes4 g8 ees bes bes bes bes |
  ees2.\fermata ees4 |
  
  aes8 aes aes aes g4 f8 f |
  des des des des c c c des |
  ees ees ees ees ees4 ees8 ees |
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


