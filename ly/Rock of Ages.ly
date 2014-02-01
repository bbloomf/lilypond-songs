\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Rock of Ages"}}
  composer = \markup\oldStyleNum"Thomas Hastings (1784–1872)"
  poet = \markup\oldStyleNum"Augustus Toplady (1740–1778)"
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
       (stretchability . 50))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 f8. g16 |
  f4 d bes'8. g16 |
  f2 \bar""
  bes8. c16 |
  d4. c8 bes a |
  bes2 \bar""\break

  a8. bes16 |
  c4. c8 a f |
  bes2 \bar""
  a8. bes16 |
  c4. c8 a f |
  bes2 \bar""\break

  f8. g16 |
  f4 d bes'8. g16 |
  f2 \bar""
  bes8. c16 |
  d4. c8 bes a |
  bes2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Rock of A -- ges, cleft for me,
  Let me hide my -- self in thee!
  Let the Wa -- ter, and the Blood,
  From thy wound -- ed Side which flow’d,
  Be of Sin the dou -- ble Cure,
  Cleanse me from its Guilt and Pow’r.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Not the La -- bor of my Hands
  Can ful -- fil thy Law’s De -- mands;
  Could my Zeal no Res -- pite know,
  Could my Tears for -- ev -- er flow,
  All for Sin could not a -- tone,
  Thou must save, and thou a -- lone.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  No -- thing in my Hand I bring,
  Simp -- ly to thy Cross I cling;
  Nak -- ed come to thee for Dress,
  Help -- less look to thee for Grace;
  Foul, I to the Foun -- tain fly;
  Wash me, Sav -- iour or I die!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  While I draw this fleet -- ing Breath,
  When my Eye -- lids close in Death,
  When I soar to Worlds un -- known,
  See thee on thy Judg -- ment Throne,
  Rock of A -- ges, cleft for me,
  Let me hide my -- self in thee.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8. ees16 |
  d4 bes d8. ees16 |
  d2
  f8. g16 |
  f4. ees8 d c |
  d2

  c8. d16 |
  ees4. ees8 ees ees |
  d2
  c8. d16 |
  ees4. ees8 ees ees |
  d2

  d8. ees16 |
  d4 bes d8. ees16 |
  d2
  f8. g16 |
  f4. ees8 d c |
  d2 \bar"|."
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
  bes8. bes16 |
  bes4 f f8. bes16 |
  bes2
  bes8. bes16 |
  bes4. f8 f f |
  f2 \bar""

  f8. f16 |
  f4. f8 f f |
  f2
  f8. f16 |
  f4. f8 f <f \tweak #'font-size #-2 c'> <f \tweak #'font-size #-2 bes>2

  bes8. bes16 |
  bes4 f f8. bes16 |
  bes2
  bes8. bes16 |
  bes4. f8 f f |
  f2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,8. bes16 |
  bes4 bes bes8. bes16 |
  bes2
  d8. ees16 |
  f4. f8 f, f |
  bes2 \bar""

  f'8. f16 |
  f4. f8 f, f |
  bes2
  f'8. f16 |
  f4. f8 f, f |
  bes2

  bes8. bes16 |
  bes4 bes bes8. bes16 |
  bes2
  d8. ees16 |
  f4. f8 f, f |
  bes2 \bar"|."
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
