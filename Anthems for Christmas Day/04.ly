\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Christmas Carrol"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
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
  \key g\major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
  \partial 4 g'4 |
  g d' d8[ c] |
  b4~ b b8[ a] |
  g4 b a8[ g] |
  g2 \bar""

  g4 |
  g d' d8[ c] |
  b4 b b8[ a] |
  g4 b a8[ g] |
  g2 \bar""

  \repeat unfold 2 {
    b4 |
    a4 a c |
    b g g |
    b d cis |
    d2 \bar""

    b4 |
    c8.[ d16] e8[ d] c4 |
    d d b8[ a] |
    g4 b a8[ g] |
  }
  \alternative {
    {g2\bar""\break}
    {g2.\bar"|."}
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  A Vir -- gin most pure, the Pro -- phet fore -- told,
  Should bring forth a Sav -- iour which now we be -- hold,
  To be our Re -- deem -- er from Death, Hell, and Sin,
  Which A -- dam’s Trans -- gres -- sion in -- vol -- ved us in.

  Then let us be Mer -- ry, cast Sor -- row a -- way,
  Our Sav -- iour Christ Je -- sus was born on this Day.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Through Beth -- le -- hem \set ignoreMelismata = ##t Ci -- ty, \unset ignoreMelismata in Ju -- ry it was,
  That Jo -- seph and Ma -- ry to -- geth -- er did pass;
  And for to be Tax -- ed with Ma -- ry Straight -- ways,
  Old Cæ -- sar com -- mand -- ed, he quick -- ly o -- beys.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  But Ma -- ry’s full \set ignoreMelismata = ##t Time be -- \unset ignoreMelismata ing come as we find,
  She brought forth her First -- born to save all Man -- kind;
  The Inn be -- ing full; for this Heav -- en -- ly Guest,
  No place there was found where to lay him to rest.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  But Ma -- ry, blest \set ignoreMelismata = ##t Ma -- ry, \unset ignoreMelismata so meek and so mild,
  Soon wrapt up in Swad -- lings this Heav -- en -- ly Child;
  Con -- tent -- ed, she laid him where Ox -- en did feed,
  The great God of Na -- ture ap -- prov’d of the Deed.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  To teach us Hu -- \set ignoreMelismata = ##t mil -- i -- \unset ignoreMelismata ty, all this was done,
  Then learn we from hence haugh -- ty Pride for to shun;
  A Man -- ger’s his Cra -- dle, who came from A -- bove,
  The great God of Mer -- cy, of Peace, and of Love.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  The pres -- ent -- ly \set ignoreMelismata = ##t af -- ter \unset ignoreMelismata the Shep -- herds did spy,
  Vast Num -- bers of An -- gels to stand in the Sky;
  So mer -- ri -- ly Talk -- ing, so sweet they did Sing,
  All Glo -- ry and Praise to our Heav -- en -- ly King.
}

altoMusic = \relative c' {
  b4 |
  d d fis |
  g4~ g d4 |
  e e d |
  b2 \bar""

  b4 |
  d d fis |
  g g d |
  e e d |
  b2 \bar""

  \repeat unfold 2 {
    b4 |
    d d e |
    d b b |
    d d e |
    fis2 \bar""

    g8[ fis] |
    e4 e g |
    fis d d |
    e e d |
  }
  \alternative {
    {b2}
    {b2.}
  }
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
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \oneVoice
  g,4 |
  g' g d |
  e4~ e g8[ fis] |
  e4 c d |
  g,2 \bar""

  g4 |
  g' g d |
  e e g8[ fis] |
  e4 c d |
  g,2 \bar""

  \repeat unfold 2 {
    g4 |
    d' d c |
    g' g e |
    b' b a |
    d,2 \bar""

    e8[ d] |
    c4 c e |
    d d g8[ fis] |
    e4 c d |
  }
  \alternative {
    {g,2}
    {g2.}
  }
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
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsVI
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
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
