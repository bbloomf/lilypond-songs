\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Bonnie Blue Flag"}}
  composer = \markup\oldStyleNum{"Folk Song," \italic "The Irish Jaunting Car"}
  poet = \markup\oldStyleNum"Harry McCarthy, 1861"
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
	\partial 8
  c8 |
  \tieDashed a'4~ a8 a4 a8 |
  bes bes~ bes bes4 bes8 |
  \tieSolid c4 c8 a4 f8 |
  g4.~ g4 \teeny c8 \normalsize |
  \tieDashed f4~ f8 e4 f8 |
  
  d4( c8) a4 f8 |
  a4 g8 f4 e8 |
  \tieSolid f4.~ f4 c8 |
  a'4 a8 a4 a8 |
  \tieDashed bes~ bes8~ bes bes4 bes8 |
  c4 c8 a4 f8 |
  
  \tieSolid g4.~ g4 c8 |
  \tieDashed f4~ f8 e4 f8 |
  \tieSolid d d c a4 f8 |
  a4 g8 f4 e8 |
  f4.~ f4 \bar"||" c'8 |
  f4.~ f4 c8 |
  
  e4.~ e4 c8 |
  d4 d8 d4 d8 |
  c4.~ c4 c8 |
  f4. c4 c8 |
  e e d c4 bes8 |
  a4 bes8 a4 g8 |
  f4.~ f4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  We are a band of broth -- ers, __ _ "" and na -- tive to the soil, __ _
  "" Fight -- ing for our Lib -- er -- ty, With trea -- sure, blood, and toil; __ _
  And when our rights were threat -- ened, _ "" the cry rose near and far, __ _
  Hur -- rah __ _ for the Bon -- nie Blue Flag that bears a sin -- gle star. __ _
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  As long __ _ as the U -- nion _ "" was faith -- ful to her trust, __ _
  Like friends _ and like Bro -- _ thers, "" kind were we and just; __ _
  But now, when North -- ern treach -- e -- ry "" at -- tempts our rights to mar, __ _
  We hoist on high the Bon -- nie Blue Flag that bears a sin -- gle star! __ _
  
  \unset ignoreMelismata
  Hur -- rah! __
  Hur -- rah! __
  For South -- ern rights hur -- rah! __
  Hur -- rah for the Bon -- nie Blue Flag that bears a sin -- gle star!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Then here’s to our Con -- fed -- e -- ra -- cy, so strong we are and brave, _
  Like pa -- tri -- ots of old we’ll fight, our her -- i -- tage to save: __ _
  And ra -- ther than sub -- mit __ _ to shame, to die we would pre -- fer, __ _
  So cheer __ _ for the Bon -- nie Blue Flag that bears a sin -- gle star. __ _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 |
  \tieDashed f4~ f8 f4 f8 |
  e e~ e e4 e8 |
  \tieSolid a4 a8 f4 f8 |
  e4.~ e4 \teeny bes'8 \normalsize |
  \tieDashed a4~ a8 g4 a8 |
  
  f4~ f8 f4 f8 |
  f4 e8 f4 c8 |
  \tieSolid c4.~ c4 c8 |
  f4 f8 f4 f8 |
  \tieDashed e~ e~ e e4 e8 |
  f4 f8 f4 c8 |
  
  \slurSolid f4.( e4) bes'8 |
  \tieDashed a4~ a8 g4 a8 |
  \tieSolid f f f f4 f8 |
  f4 c8 c4 c8 |
  \tieSolid c4.~ c4 \bar"||"
  f8 a4.~ a4 a8 |
  
  g4.~ g4 g8 |
  f4 f8 f4 f8 |
  f4.~ f4 f8 |
  a4. f4 f8 |
  f f f d4 d8 |
  c4 c8 e4 e8 |
  f4.~ f4 \bar"|."
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
  a8 |
  \tieDashed c4~ c8 c4 c8 |
  c c~ c c4 c8 |
  \tieSolid c4 c8 c4 a8 |
  c4.~ c4 \teeny c8 \normalsize |
  \tieDashed c4~ c8 c4 c8 |
  
  bes4( a8) c4 a8 |
  c4 bes8 a4 g8 |
  \tieSolid a4.~ a4 a8 |
  c4 c8 c4 c8 |
  \tieDashed c~ c~ c c4 c8 |
  c4 c8 c4 a8 |
  
  \tieSolid c4.~ c4 c8 |
  \tieDashed c4~ c8 c4 c8 |
  \tieSolid bes bes a c4 a8 |
  c4 bes8 a4 g8 |
  a4.~ a4 \bar"||"
  a8 |
  c4.~ c4 c8 |
  
  c4.~ c4 c8 |
  bes4 bes8 bes4 bes8 |
  a4.~ a4 a8 |
  c4. a4 a8 |
  bes bes bes g4 g8 |
  f4 f8 c'4 bes8 |
  a4.~ a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8 |
  \tieDashed f4~ f8 f4 f8 |
  g g~ g g4 g8 |
  \tieSolid f4 f8 f4 f8 |
  c4.~ c4 \teeny e8 \normalsize |
  \tieDashed f4~ f8 c4 f8 |
  
  f4~ f8 f4 f8 |
  c4 c8 c4 c8 |
  \tieSolid f4.~ f4 f8 |
  f4 f8 f4 f8 |
  \tieDashed g~ g~ g g4 g8 |
  a4 a8 f4 f8 |
  
  \tieSolid c4.~ c4 e8 |
  \tieDashed f4~ f8 c4 f8 |
  \tieSolid f f f f4 f8 |
  c4 c8 c4 c8 |
  f4.~ f4 \bar"||"
  f8 |
  f4.~ f4 f8 |
  
  c4.~ c4 e8 |
  f4 f8 f4 f8 |
  f4.~ f4 f8 |
  f4. f4 f8 |
  bes, bes bes bes4 bes8 |
  c4 c8 c4 c8 |
  f4.~ f4 \bar"|."
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
    \tempo 4 = 160
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
