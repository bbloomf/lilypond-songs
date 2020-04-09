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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #193
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
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \repeat volta 2 {
    f2 f4 g |
    a2 g4( a) |
    bes2 a4( g) |
    a1 |
    a2 a4 a |
    bes2 a4( g) |
    f2 e4( f) |
    g1 |

    f2 f4 g |
    a2 g4( a) |
    bes2 a4( g) |
    a1 |
    a2 a4 a |
    g2 f |
    g a4( g) |
    f1
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  O Thou that hear’st when sin -- ners cry,
  Though all my crimes be -- fore Thee lie,
  Be -- hold me not with an -- gry look,
  But blot their mem -- ’ry from Thy book.
  \set stanza = #"5. "
  A bro -- ken heart, my God, my King,
  Is all the sac -- ri -- fice I bring;
  The God of grace will ne’er de -- spise
  A bro -- ken heart for sac -- ri -- fice.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Cre -- ate my na -- ture pure with -- in,
  And form my soul a -- verse to sin:
  Let Thy good Spi -- rit ne’er de -- part,
  Nor hide Thy pre -- sence from my heart.
  \set stanza = #"6. "
  My soul lies hum -- bled in the dust,
  And owns Thy dread -- ful sen -- tence just;
  Look down, o Lord, with pit -- ying Eye,
  And save the soul con -- demn’d to die.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  I can -- not live wit -- hout Thy light,
  Cast out and ban -- ish’d from Thy sight:
  Thy sav -- ing strength, o Lord re -- store,
  And guard me that I fall no more.
  \set stanza = #"7. "
  Then will I teach the world Thy ways;
  Sin -- ners shall learn Thy sov -- ’reign grace;
  I’ll lead them to my Sav -- iour’s blood,
  And they shall praise a pard -- ’ning God.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Though I have griev’d Thy Spi -- rit, Lord,
  His help and com -- fort still af -- ford;
  And let a wretch come near Thy throne,
  To plead the mer -- its of Thy Son.
  \set stanza = #"8. "
  O may Thy love in -- spire my tongue!
  Sal -- va -- tion shall be all my song;
  And all my pow’rs shall join to bless
  The Lord my strength and righ -- teous -- ness.
}

sopWordsV = \lyricmode {
}

sopWordsVI = \lyricmode {
}
sopWordsVII = \lyricmode {
}
sopWordsVIII = \lyricmode {
}

altoMusic = \relative c' {
  c2 c4 e |
  f2 g4( fis) |
  g2 f?4( e) |
  f1 |
  f2 f4 f |
  f2 f4( e) |
  f2 c |
  e1 |

  c2 c4 e |
  f2 g4( fis) |
  g2 f?4( e) |
  f1 |
  f2 f4 f |
  e2 f |
  f e |
  f1
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
  a2 a4 c |
  c2 d |
  d c |
  c1 |
  c2 c4 c |
  d2 c |
  c bes4( a) |
  c1 |

  a2 a4 c |
  c2 d4( c) |
  d2 c |
  c1 |
  c2 c4 c |
  c( bes) a2 |
  d c4( bes) |
  a1
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,2 f4 c |
  f2 bes4( a) |
  g2 c, |
  f1 |
  f2 f4 f |
  f2 f4( bes) |
  a2 g4( f) |
  c1 |

  f2 f4 c |
  f2 bes4( a) |
  g2 c, |
  f1 |
  f2 f4 f |
  c2 d |
  bes c |
  f1
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
      \new Voice = "sopranos" { \voiceOne \global \sopMusic\sopMusic }
      \new Voice = "altos" { \voiceTwo \global \altoMusic\altoMusic }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
    \new Lyrics = "altosVI"  \lyricsto "sopranos" \sopWordsVI
    \new Lyrics = "altosVII"  \lyricsto "sopranos" \sopWordsVII
    \new Lyrics = "altosVIII"  \lyricsto "sopranos" \sopWordsVIII
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne \global \tenorMusic\tenorMusic }
      \new Voice = "basses" { \voiceTwo \global \bassMusic\bassMusic }
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"O Thou that hear’st when sinners cry"}}
  composer = \markup\oldStyleNum"Lowell Mason (1792–1872)"
  poet = \markup\oldStyleNum"Isaac Watts (1674–1748)"
  tagline = ""
}}
