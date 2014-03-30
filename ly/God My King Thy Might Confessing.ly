\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"God My King Thy Might Confessing"}}
  composer = \markup\oldStyleNum"Felix Mendelssohn (1809–1847)"
  poet = \markup\oldStyleNum"Richard Mant (1776–1848)"
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4 f g f |
  bes d8[ c] bes4 a |
  g a bes ees |
  d c bes2 |

  f4 f g f |
  bes d8[ c] bes4 a |
  bes a8[ g] f4 bes |
  d c bes2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  God, my King, Thy might con -- fess -- ing,
  Ev -- er will I bless Thy Name;
  Day by day Thy throne ad -- dress -- ing,
  Still will I Thy praise pro -- claim.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Hon -- or great our God be -- fit -- teth;
  Who His Ma -- jes -- ty can reach?
  Age to age His works trans -- mit -- teth,
  Age to age His pow’r shall teach.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  They shall talk of all Thy glo -- ry,
  On Thy might and great -- ness dwell,
  Speak of Thy dread acts the sto -- ry,
  And Thy deeds of won -- der tell.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Nor shall fail from mem -- ’ry’s trea -- sure,
  Works by love and mer -- cy wrought,
  Works of love sur -- pass -- ing mea -- sure,
  Works of mer -- cy pass -- ing thought.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Full of kind -- ness and com -- pas -- sion,
  Slow to an -- ger, vast in love,
  God is good to all cre -- a -- tion;
  All His works His good -- ness prove.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  All Thy works, O Lord, shall bless Thee;
  Thee shall all Thy saints a -- dore;
  King su -- preme shall they con -- fess Thee,
  And pro -- claim Thy sov -- ’reign pow’r.
}

altoMusic = \relative c' {
  d4 f ees8[ d] c4 |
  bes g' f f |
  f8[ e] f4 f ees |
  f8[ bes] a[ g] f2 |

  f4 d ees ees |
  d8[ f] bes[ a] g4 fis |
  g f?8[ ees] d4 f8[ g] |
  f4 f8[ ees] d2 \bar"|."
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
  bes4 bes bes a |
  bes8[ f'] f[ ees] d4 c |
  c c bes bes |
  bes8[ d] f[ ees] d2 |

  d4 d8[ c] bes4 c |
  d d8[ ees] d4 d |
  d c d bes |
  bes a bes2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,4 d ees ees |
  d ees f f |
  c f8[ ees] d4 g |
  f f bes,2 |

  bes'4 aes g a |
  bes bes,8[ c] d4 d |
  g, a bes d8[ ees] |
  f4 f bes,2 \bar"|."
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
    \new Lyrics = "altosVI"  \lyricsto "sopranos" \sopWordsVI
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
