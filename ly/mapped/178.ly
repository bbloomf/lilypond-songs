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
  first-page-number = #178
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
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'4 d a b |
  a4. g8 fis2 |
  a4 a g fis |
  e e d2 |

  a'4 d a b |
  a4. g8 fis2 |
  a4 a g fis |
  e e d2 |

  e4 e fis8[ gis] a4 |
  a gis a2 |
  b4. cis8 d4 d |
  cis cis b2 |

  fis4 fis b a |
  a gis a2 |
  b4 a g fis |
  e e d2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  At the Lamb’s high feast we sing
  Praise to our vic -- to -- rious King,
  Who hath washed us in the tide
  Flow -- ing from His pierc -- èd side;
  Praise we Him, whose love di -- vine,
  Gives His sa -- cred Blood for wine,
  Gives His Bod -- y for the feast,
  Christ the Vic -- tim, Christ the Priest.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Where the Pas -- chal blood is poured,
  Death’s dark an -- gel sheaths his sword
  Is -- rael’s hosts tri -- umph -- ant go
  Through the wave that drowns the foe.
  Praise we Christ, Whose Blood was shed,
  Pas -- chal Vic -- tim, Pas -- chal Bread;
  With sin -- cer -- i -- ty and love
  Eat we man -- na from a -- bove.
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Might -- y Vic -- tim from on high!
  Pow’rs of hell be -- neath Thee lie;
  Death is bro -- ken in the fight,
  Thou hast brought us life and light:
  Now Thy ban -- ner Thou dost wave,
  Con -- quering Sa -- tan and the grave.
  See the prince of dark -- ness quell’d;
  Heav’n’s bright gates are o -- pen held.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Pas -- chal tri -- umph, Pas -- chal joy,
  Sin a -- lone can this de -- stroy;
  From sin’s death do Thou set free
  Souls re -- born, dear Lord, in Thee.
  Hymns of glo -- ry, songs of praise,
  Fa -- ther, un -- to Thee we raise;
  Ris -- en Lord, all praise to Thee,
  With the Spir -- it, ev -- er be.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 d d d |
  d cis d2 |
  d4 d8[ cis] b[ cis] d4 |
  d cis d2 |

  d4 d d d |
  d cis d2 |
  d4 d8[ cis] b[ cis] d4 |
  d cis d2 |

  cis4 cis d cis |
  fis e8[ d] cis2 |
  e8[ fis] g4 fis fis |
  fis fis8[ e] d2 |

  d4 d d cis |
  d d cis2 |
  d4 d8[ cis] b[ cis] d4 |
  d cis d2 \bar"|."
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
  fis,4 fis8[ g] a4 g8[ fis] |
  e[ d] e4 d2 |
  fis4 fis g a |
  b a fis2 |

  fis4 fis8[ g] a4 g8[ fis] |
  e[ d] e4 d2 |
  fis4 fis g a |
  b a fis2 |

  a4 a a a |
  b b a2 |
  g4. a8 b4 b |
  b ais b2 |

  a4 a g8[ fis] e4 |
  d8[ fis] e[ d] e2 |
  g4 fis g a |
  b a8[ g] fis2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,4 b fis g |
  a a d2 |
  d4 d e fis |
  g8[ e] a4 d,2 |

  d4 b fis g |
  a a d2 |
  d4 d e fis |
  g8[ e] a4 d,2 |

  a4 a d fis8[ e] |
  d4 e a,2 |
  e'4 e b8[ cis] d[ e] |
  fis4 fis b,2 |

  d4 d g, a |
  b b a2 |
  g4 d' e fis |
  g8[ e] a4 d,2 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"At the Lamb’s High Feast We Sing"}}
  composer = \markup\oldStyleNum"Jacob Hintze (1622–1702)"
  arranger = \markup\oldStyleNum"Harmonized by Johann Sebastian Bach (1685–1750)"
  poet = \markup\oldStyleNum"From 7th century Latin"
  meter = \markup\oldStyleNum"Translated by Robert Campbell (1814–1868)"
  tagline = ""
}}
