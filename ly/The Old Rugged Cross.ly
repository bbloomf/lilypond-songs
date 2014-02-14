\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Old Rugged Cross"}}
  composer = \markup\oldStyleNum"George Bennard (1873–1958)"
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
       (padding . 1)
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
  \key bes \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
}

sopMusic = \relative c' {
	\partial 8 d16 ees |
  f8. e16 g8 f4 f16 f |
  g8. fis16 a8 g4 g16[ g] |
  a8.( g16) f8

  ees f ees |
  d4.~ d4 d16 ees |
  f8. e16 g8 f4 f16 f |
  g8. fis16 a8 g4 g16[ g] |
  
  a8. g16 f8 ees' d c |
  bes4.~ bes4 \bar"||"\break

  a16 bes |
  c8. c16 c8 c bes a |

  bes4.~ bes4 bes16 a |
  g8. g16 g8 bes a g |
  f4.~ f4 \bar""\break f16 bes |
  d8. d16 d8

  d ees d |
  g,4.~ g4 ees'16 ees |
  d8. c16 bes8 f a c |
  bes4.~ bes4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  On a hill far a -- way stood an old rug -- ged cross,
  The _ em -- blem of suff -- ’ring and shame; __ _
  And I love that old cross where the dear -- est and best
  For a world of lost sin -- ners was slain. __ _

  \unset ignoreMelismata
  So I’ll cher -- ish the old rug -- ged cross, __
  Till my tro -- phies at last I lay down;
  I will cling to the old rug -- ged cross, __
  And ex -- change it some day for a crown.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Oh, that old rug -- ged cross, so de -- spised by the world,
  Has a won -- drous at -- trac -- tion for me; __ _
  For the dear Lamb of God left His glo -- ry a -- bove
  To __ _ bear it to dark Cal -- va -- ry. __ _
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  In that old rug -- ged cross, stained with blood so di -- vine,
  A __ _ won -- _ drous beau -- ty I see, __ _
  For ’twas on that old cross Je -- sus suf -- fered and died,
  To __ _ par -- don and sanc -- ti -- fy me. __ _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  To the old rug -- ged cross I will ev -- er be true;
  Its __ _ shame and re -- proach glad -- ly bear; __ _
  Then He’ll call me some day to my home far a -- way,
  Where His glo -- ry for -- ev -- er I’ll share. __ _

  \unset ignoreMelismata
  \set associatedVoice = "basses"
  So I’ll cher -- ish the cross, the old rug -- ged cross,

  \repeat unfold 9 ""
  I will cling to the cross, the old rug -- ged cross,
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \tieDashed
  bes16 c |
  d8. cis16 cis8 d4 d16 d |
  ees8. ees16 ees8 ees4 e16[ e] |
  ees!8.~ ees16 d8

  c c c |
  \tieSolid
  bes4.~ bes4 bes16 c |
  d8. cis16 cis8 d4 d16 d |
  ees8. ees16 ees8 ees4

  e16[ e] |
  ees!8. ees16 ees8 g f ees |
  d4.~ d4 \bar"||"

  c16 d |
  ees8. ees16 ees8 ees4 ees8 |

  d cis ees d4 f16 f |
  ees8. ees16 ees8 g f ees |
  d4.~ d4 d16 d |
  f8. f16 f8

  f[ g] f |
  ees ees ees ees4 g16 g |
  f8. ees16 d8 ees ees ees |
  d4.~ d4 \bar"|."
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
  f,16 f |
  bes8. bes16 g8 bes4 bes16 bes |
  bes8. a16 c8 bes4 c16[ c] |
  c8.( a16) bes8

  a8 a f |
  f4.~ f4 f16 f |
  bes8. bes16 g8 bes4 bes16 bes |
  bes8. a16 c8 bes4

  c16[ c] |
  c8. bes16 a8 a bes a |
  bes4.~ bes4 \bar"||"

  f16 f |
  a8. a16 a8 a4 f8 |

  f8 e g f4 bes16 bes |
  bes8. bes16 bes8 bes bes bes |
  bes4.~ bes4 bes16 bes |
  bes8. bes16 bes8

  bes4 bes8 |
  bes bes bes bes4 bes16 bes |
  bes8. a16 bes8 a c f, |
  f4.~ f4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \tieDashed
  bes,16 bes |
  bes8. bes16 bes8 bes4 bes16 bes |
  ees8. ees16 ees8 ees4 c16[ c] |
  f8.~ f16 f8

  f f f |
  \tieSolid
  bes,4.~ bes4 bes16 bes |
  bes8. bes16 bes8 bes4 bes16 bes |
  ees8. ees16 ees8 ees4

  c16[ c] |
  f8. f16 f8 f f f |
  bes,4.~ bes4 \bar"||"

  f'16 f |
  f8. f16 f8 f4 f8 |

  bes,8 bes bes bes4 d16 d |
  ees8. ees16 ees8 ees ees ees |
  bes4.~ bes4 bes16 bes |
  bes8. bes16 bes8

  bes4 bes8 |
  ees8 ees ees ees4 ees16 ees |
  f8. f16 f8 f f f |
  bes,4.~ bes4 \bar"|."
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
    \new Lyrics = "altosIV"
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altosIV" \lyricsto "sopranos" \sopWordsIV
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
