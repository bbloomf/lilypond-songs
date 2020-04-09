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
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -13)
       (stretchability . 80))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #56
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
  \key des \major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 2
  f2 |
  ees4. f8 f2 f |
  ees4. f8 f2 f |
  f4 ges aes2 ges |
  
  f ees\fermata ees |
  d4. ees8 ees2 ees |
  d4. ees8 ees2 ees |
  ees4 f ges2 f |
  
  ees des\fermata aes'|
  bes4. aes8 aes2 aes |
  bes4. aes8 aes2 aes |
  ees'2. c4 aes ges |
  
  ges2 f b4\rest f |
  ees4. ges8 bes2 b4\rest c, |
  des4. f8 aes2 b4\rest c, |
  bes ges' f2 ees |
  ees des \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	O calm of night, when stars shone bright,
  A soft voice sad -- ly sing -- ing.
  The winds that blow, re -- ech -- o low
  The sad tones sweet -- ly bring -- ing;
  
  There’s no re -- lief from woe and grief,
  My heart’s in sor -- row seek -- ing
  The one who’s gone; pain lin -- gers on,
  Haunts me a -- wake or sleep -- ing.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The gold -- en moon is sink -- ing soon,
  It can -- not glow for sor -- row.
  No more at night the stars shine bright,
  My pain they too would bor -- row;
  
  No more we’ll stray through mead -- ows gay;
  I pass my days in weep -- ing.
  For love I yearn; till its re -- turn
  My vi -- gil I’ll be keep -- ing.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  des2 |
  des4. des8 des2 des |
  des4. des8 des2 des |
  des4 des des2 ees |
  
  des c c |
  b4. c8 c2 c |
  b4. c8 c2 c |
  c4 c c2 des |
  
  c des f |
  g4. f8 f2 f |
  g4. f8 f2 aes |
  aes2. ges?4 f ees |
  
  ees2 des s4 des |
  des4. ees8 ees2 s4 c |
  des4. des8 des2 s4 aes |
  bes ees des2 c |
  c des \bar"|."
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
  aes2 |
  beses4. aes8 aes2 aes |
  beses4. aes8 aes2 des |
  des4 bes f2 bes |
  
  aes2 aes ges |
  f4. ges8 ges2 ges |
  f4. ges8 ges2 ges |
  ges4 f ees2 f |
  
  ges f des'|
  e4. f8 f2 des |
  e4. f8 f2 f |
  ges2. ees?4 des ees |
  
  c2 des s4 des |
  bes4. bes8 bes2 s4 c |
  aes4. aes8 aes2 s4 aes |
  ges bes aes2 ges |
  ges f \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  des,2 |
  ges,4. des'8 des2 des |
  ges,4. des'8 des2 des |
  des4 ges des2 ges|
  aes aes\fermata aes, |
  aes4. aes8 aes2 aes |
  aes4. aes8 aes2 aes |
  aes4 aes aes2 aes |
  
  aes des\fermata des'|
  c4. des8 des2 des |
  c4. des8 des2 des |
  c2. aes4 bes c |
  
  a2 bes d,4\rest bes' |
  ges4. ges8 ges2 d4\rest aes'|
  f4. f8 f2 d4\rest f |
  ges ees aes2 aes, |
  aes des \bar"|."
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
      \new Voice = "sopranos" \transpose des ees { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" \transpose des ees { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" \transpose des ees { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" \transpose des ees { \voiceTwo << \global \bassMusic >> }
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"O Calm of Night"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"(In Stiller Nacht)"}}
  composer = \markup\oldStyleNum"Swabian Folk Song"
  arranger = \markup\oldStyleNum"Arranged by Johannes Brahms (1833–1897)"
  tagline = ""
}}


global = {
  \key g\major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  d4 |
  b'2 g4 g |
  g2 e4 a8. a16 |
  g2 a |
  b2.\fermata \bar"" d,4 |
  b'2 g4 g |
  g( c) b a8. a16 |
  d2 fis, |
  g2.\fermata \bar"|."
}
sopWords = \lyricmode {
	Ein Pro -- sit, ein Pro -- sit der Ge -- müt -- lich -- keit.
  Ein Pro -- sit, ein Pro -- sit der Ge -- müt -- lich -- keit!
}

sopWordsII = \lyricmode {
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  d4 |
  d2 d4 ees |
  e?2 e4 fis8. fis16 |
  d2 fis |
  g2. d4 |
  d2 d4 ees |
  e?2 d4 fis8. fis16 |
  g2 d |
  d2. \bar"|."
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
  \partial 4
  d,4 |
  g2 g4 g |
  g2 g4 d'8. c16 |
  b2 c |
  d2. d,4 |
  g2 g4 g |
  g4( a) g d'8. c16 |
  b2 c |
  b2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  d,4 |
  g,2 b4 b |
  c2 c4 d8. d16 |
  g2 d |
  g,2.\fermata \bar"" d'4 |
  g,2 b4 b |
  c4( a) b d8. d16 |
  d2 d |
  g,2.\fermata \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Ein Prosit"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}


