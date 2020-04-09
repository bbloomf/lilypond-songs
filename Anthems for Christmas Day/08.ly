\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"An Hymn for Christmas Day"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
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
  \key c\major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 2 e'4( d) |
  c2( b) a |
  g1 c2 |
  d1 c2 |
  b1 \bar"||"
  c2 |
  d1 c2 |
  b1 c2 |
  a1 b2 |
  c1 \bar"||"

  c2 |
  a1 a2 |
  b1 c2 |
  c1 c2 |
  d1 \bar"||"
  e4( d) |
  c1 b2 |
  a1 g2 |
  c1 b2 |
  c1 \bar"||"

  e4( d) |
  c1 d2 |
  b1 a2 |
  g( c) b |
  c1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Im -- mor -- tal babe, who on this day,
  Didst change thy heav’ns for our vile clay,
  And didst with flesh thy god -- head veil:
  E -- ter -- nal son of God all hail!
  E -- ter -- nal son of God all hail!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Bright An -- gels ush -- er’d in thy Birth,
  With Ac -- cla -- ma -- tions and with Mirth;
  And sung a -- loud, that for our Sake,
  Thou didst our hu -- man Na -- ture take,
  Thou didst our hu -- man Na -- ture take.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  And shall not we, poor Crea -- tures, whom
  Thou didst from Heav’n on Pur -- pose come,
  To save, when Slaves to Sa -- tan, we
  Knew Nought but Sin and Mis -- e -- ry,
  Knew Nought but Sin and Mis -- e -- ry.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  We all thro’ Ig -- no -- rance and Vice,
  Had lost our Way to Pa -- ra -- dise;
  And had with -- out God in the World,
  Been in -- to out -- er Dark -- ness hurl’d,
  Been in -- to out -- er Dark -- ness hurl’d.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Had not that Day -- Spring from on high,
  Vis -- it -- ed Mor -- tals far and nigh;
  And Christ, our true E -- man -- u -- el,
  Stopp’d us in our Ca -- reer to Hell,
  Stopp’d us in our Ca -- reer to Hell.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  Rav -- ish’d, Lord, with this Love of thine,
  To sing thy Praise our Souls in -- cline;
  And that we may per -- form our Parts,
  We’ll to our Voi -- ces tune our Hearts,
  We’ll to our Voi -- ces tune our Hearts.
}

altoMusic = \relative c' {
  g'4( fis) |
  e1 d2 |
  c1 a'2 |
  g1 fis2 |
  g1 \bar"||"
  g2 |
  g1 g2 |
  g1 g2 |
  e1 e2 |
  f1 \bar"||"

  f2 |
  a1 fis2 |
  g1 g2 |
  e1 d2 |
  d1 \bar"||"
  g4( f) |
  e1 d2 |
  c1 g'2 |
  g1 g2 |
  g1 \bar"||"

  g2 |
  a1 fis2 |
  g1 a2 |
  g1 f2 |
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
  c2 |
  e,1 f2 |
  g1 c2 |
  b2 c1 |
  d1 \bar"||"
  e2 |
  f1 e2 |
  d1 c4( b) |
  c2 b2.( a4) |
  a1 \bar"||"

  c2 |
  c1 c2 |
  b1 a2 |
  c2 b( a) |
  g1 \bar"||"
  g2 |
  a1 b2 |
  c1 d2 |
  f e( d) |
  c1 \bar"||"

  c2 |
  a1 d2 |
  d1 e2 |
  f e( d) |
  c1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,2 |
  c1 d2 |
  e1 f2 |
  g1 a2 |
  g1 \bar"||"
  c2 |
  b1 c2 |
  g1 g2 |
  a1 a2 |
  f1 \bar"||"

  a2 |
  a1 a2 |
  g1 g2 |
  a1 g4( f) |
  g1 \bar"||"
  e2 |
  f1 g2 |
  a1 b2 |
  c1 g2 |
  c1 \bar"||"

  e,2 |
  f1 d2 |
  g1 a2 |
  c1 g2 |
  c,1 \bar"|."
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
    \new Lyrics = "altosVI"  \lyricsto "sopranos" \sopWordsVI
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
