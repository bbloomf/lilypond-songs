\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Lead Kindly Light"}}
  poet = \markup\oldStyleNum"John Henry Newman (1801–1890)"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 80))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #132
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
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 2.
  ees4 aes bes |
  c4. c8 bes4 aes f aes |
  f2( ees1) |
  aes2 g aes |
  bes2. \bar"||"
  ees,4 aes bes |
  
  c4. c8 bes4 aes f aes |
  f2( ees1) |
  ees2 aes2. g4 |
  g2( aes1) |
  bes2 bes bes |
  bes1 bes2 |
  
  bes4 c bes( aes) g( f) |
  ees2~ ees1 |
  c'2 bes aes |
  aes2. aes4 g f |
  ees2( aes2.) g4 |
  g2( aes4)\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Lead, kind -- ly Light, a -- mid th’en -- cir -- cling gloom,
  Lead Thou me on;
  The night is dark, and I am far from home,
  Lead Thou me on.
  
  Keep Thou my feet; I do not ask to see
  The dis -- tant scene; one step e -- nough for me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I was not ev -- er thus, nor prayed that Thou
  Shouldst lead me on;
  I loved to choose and see my path; but now
  Lead Thou me on.
  
  I loved the gar -- ish day; and, spite of fears,
  Pride ruled my will: re -- mem -- ber not past years.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  So long Thy pow’r has blest me, sure it still
  Will lead me on
  O’er moor and fen, o’er crag and tor -- rent, till
  The night is gone,
  
  And with the morn those an -- gel fac -- es smile,
  Which I have loved long since, and lost a -- while.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees4 ees ees |
  ees4. c8 des4 ees ees des |
  des2( ees1) |
  ees2 ees ees4( c) |
  des2. |
  ees4 ees ees |
  
  ees4. c8 des4 ees ees des |
  des2( c1) |
  c2 ees2. des4 |
  ees1. |
  ees2 d ees |
  f1 ees2 |
  
  ees4 ees d2 d |
  ees2( c des?) |
  c des ees |
  f2. f4 ees des |
  c2( ees2.) des4 |
  des2( c4) \bar"|."
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
  c4 c des |
  c4. aes8 aes4 aes aes aes |
  aes2( c des) |
  ees des c4( aes) |
  aes2( g4) |
  des' des des |
  
  c4. aes8 aes4 aes aes aes |
  aes2~ aes1 |
  aes2 c2. bes4 |
  des2( c1) |
  bes2 aes g4( c) |
  bes2( aes) g |
  
  g4 g aes2 aes |
  g( aes bes) |
  aes f4( g) aes2 |
  aes2. f4 g aes |
  aes2( c2.) bes4 |
  bes2( aes4) \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes4 aes aes |
  aes4. aes,8 bes4 c des f |
  aes2~ aes( bes2) |
  c bes aes |
  ees2. |
  ees4 f g |
  
  aes4. aes,8 bes4 c des f |
  aes2( aes, c) |
  ees ees2. ees4 |
  ees2( aes1) |
  g2 f ees |
  d1 ees2 |
  
  bes4 bes bes2 bes |
  ees1. |
  aes,2 bes c |
  des2. des4 des des |
  ees2~ ees2. ees4 |
  ees2( aes,4)\fermata \bar"|."
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
    \tempo 4 = 105
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


