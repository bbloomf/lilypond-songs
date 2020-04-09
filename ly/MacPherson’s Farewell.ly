\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"MacPherson’s Farewell"}}
  composer = \markup\oldStyleNum"Jamie MacPherson (1675–1700)"
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
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
  ragged-last-bottom = ##t
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
  \key a \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 e4 |
  a4. b8 cis4 b8[ a] |
  b[ a] b[ cis] b4 cis8[ b] |
  a4. b8 cis4 b8[ a] |
  fis2. \bar"" e8[ fis] |

  a4. b8 cis4 b8[ a] |
  b[ a] b[ cis] b4 cis8[ b] |
  a4. cis8 b[ a] fis4 |
  e2. \bar"||"\break cis'8[ d] |

  e4. cis8 d[ cis] b[ a] |
  cis4 b b cis8[ d] |
  e4. cis8 d[ cis] b[ a] |
  fis2. \bar""\break fis'4 |
  e4. cis8 d[ cis] b[ a] |
  cis4 b b e, |
  a4. cis8 b[ a] fis4 |
  e2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Fare -- well, ye dun -- geons dark and strong,
    The wretch -- ’s des -- ti -- nie!
  Mac -- Pher -- son’s time will not be long
    On yon -- der gal -- lows tree.

  Sae ran -- ting -- ly, sae wan -- ton -- ly,
    Sae daun -- ting -- ly gaed he;
  He play’d a spring, an’ danc’d it round,
    Be -- low the gal -- lows tree.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh, what is death but part -- ing breath?
    On mo -- nie~a blu -- die plain
  I’ve dared his face, and in this place
    I scorn him yet a -- gain!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Un -- tie these bands from off my hands,
    And bring to me my sword;
  \set ignoreMelismata = ##t
  And there’s \unset ignoreMelismata not a man in all Scot -- land,
  \set ignoreMelismata = ##t
    But I’ll \unset ignoreMelismata brave him at a word.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  I’ve lived a life of sturt and strife;
    I die by trea -- cher -- ie:
  It burns my heart I must de -- part,
    And not a -- ven -- ged be.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Now fare -- well, light, thou sun -- shine bright,
    And all be -- neath the sky!
  May cow -- ard shame dis -- tain his name,
    The wretch that dares not die!
}

altoMusic = \relative c' {
  e4 |
  cis4. cis8 e4 d8[ e] |
  fis4 fis fis fis |
  e4. cis8 e4 d8[ cis] |
  d2.

  e8[ d] |
  cis4. cis8 e4 d8[ e] |
  fis4 fis fis fis |
  e4. e8 fis4 d |
  e2. \bar"||"

  gis4 |
  a4. a8 e4 e |
  e4 gis gis gis |
  a4. a8 e4 e |
  d2.

  a'4 |
  a4. a8 e4 e |
  e4 gis gis e |
  e4. e8 fis4 d |
  cis2. \bar"|."
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
  e,4 |
  a4. a8 a4 gis8[ a] |
  d4 d d d |
  cis4. a8 a4 gis8[ e] |
  a2.

  e8[ a] |
  a4. a8 a4 gis8[ a] |
  d4 d d d |
  cis4. cis8 d4 a |
  gis4( a b) \bar"||"

  e4 |
  cis4. a8 a4 a |
  a4 b b e |
  cis4. a8 a4 a |
  a2.

  d4 |
  cis4. a8 a4 a |
  a4 b b gis |
  a4. cis8 d4 a |
  a2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 |
  a,4. a8 a4 b8[ cis] |
  d4 d d d |
  a4. a8 a4 b8[ cis] |
  d2.

  e8[ d] |
  a4. a8 a4 b8[ cis] |
  d4 d d d |
  a4. a8 d4 d |
  e4( fis gis)

  e4 |
  a,4. a8 a4 b8[ cis] |
  e4 e e e |
  a,4. a8 a4 b8[ cis] |
  d2.

  d4 |
  a4. a8 a4 b8[ cis] |
  e4 e e e8[ d] |
  cis4 a d d |
  a2. \bar"|."
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
