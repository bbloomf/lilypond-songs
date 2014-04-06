\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Sweet the moments rich in blessing"}}
  composer = \markup\oldStyleNum"Isaac Baker Woodbury (1819–1858)"
  poet = \markup\oldStyleNum"James Allen (1734–1804)"
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
  \key e\major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 2
  gis'4 gis |
  gis2 e fis4 fis |
  gis2 gis b4 cis |
  b2 gis e4 gis
  fis1 \bar""
  
  gis4 gis |
  gis2 e fis4 fis |
  gis2 gis b4 cis |
  b2 e, gis4 fis |
  e1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Sweet the mo -- ments, rich in bless -- ing,
  Which be -- fore the cross I spend;
  Life and health and peace pos -- sess -- ing
  Through the sin -- ner’s dy -- ing friend.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Here I kneel in won -- der, view -- ing
  Mer -- cy poured in streams of blood;
  Pre -- cious drops, for par -- don su -- ing,
  Make and plead my place with God.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Tru -- ly bless -- èd is the sta -- tion,
  Low be -- fore His cross to lie,
  While I see di -- vine com -- pas -- sion
  Plead -- ing in His dy -- ing eye.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Here I find my hope of heav -- en,
  While up -- on the Lamb I gaze;
  Lov -- ing much, and much for -- giv -- en,
  Let my heart o’er -- flow with praise.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Lord, in lov -- ing con -- tem -- pla -- tion
  Fix my heart and eyes on Thee,
  Till I taste Thy full sal -- va -- tion,
  And Thine un -- veiled glo -- ries see.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  For Thy sor -- rows I a -- dore Thee,
  For the griefs that wrought our peace;
  Gra -- cious Sav -- ior, I im -- plore Thee,
  In my heart Thy love in -- crease.
}

altoMusic = \relative c' {
  e4 e |
  e2 e dis4 dis |
  e2 e gis4 a |
  gis2 e e4 e |
  dis1

  e4 e |
  e2 e dis4 dis |
  e2 e gis4 a |
  gis2 e e4 dis |
  e1 \bar"|."

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
  b4 b |
  b2 b b4 b |
  b2 b e4 e |
  e2 b b4 b |
  b1

  b4 b |
  b2 b b4 b |
  b2 b e4 e |
  b2 b b4 a |
  gis1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 e |
  e2 gis b4 b |
  e,2 e e4 e |
  e2 e gis4 e |
  b1

  e4 e |
  e2 gis b4 b |
  e,2 e e4 e |
  e2 gis b4 b, |
  e1 \bar"|."
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
