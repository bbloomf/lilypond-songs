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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #80
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
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	g'4 g8. g16 c4 c, |
  e2 d4 b'\rest |
  b\rest d, f e8 d |
  e2. b'4\rest |
  e,4\< e8 e\! fis4 g8[ a] |
  b2 g4 b\rest |
  
  d4 b8 g a4. a8 |
  g2. g4 |
  e d8[ cis] d4 b |
  a'4 g8[ fis] g fis\< g a\! |
  b4 g8[ a] b4-> b-> |
  b2. a8 g |
  
  e'4 d c e,8 e |
  c'4 b a2 |
  e4 f8 e a4 a |
  d,4 e8[ d] g2 |
  f4 e8 e d4 c |
  g'2( f4) d |
  c2. b'4\rest \bar"||"
  
  %verse2
  e,4 e8 e a4 c |
  c2 b4 b\rest |
  a2 a4 g8 f |
  e2. b'4\rest |
  e, e8 e f4 e |
  
  a2 a4 b\rest |
  c\> b8 a gis4 a\! |
  b2. b4\rest |
  b,4 b8 b c4 b |
  e2. e4 |
  e d8[ cis] d4 b |
  
  a'4 g8.[ fis16] g8\< fis g a |
  b4\! g8[ a] b4 b |
  b2. a8[ g] |
  e'4 d c e, |
  c' b a2 |
  e4 f8 e a4 a |
  
  %page2
  d,4( e8) d g2 |
  f4 e8 e d4 c |
  g'2( f4) d4\fermata |
  c2. b'4\rest |
  %verse3
  g4\f g8 g c4 c, |
  e2 d4 b'\rest |
  
  a2 a4 g8 f |
  e2. b'4\rest |
  e,4\< e8 e fis4\! g8[ a] |
  b2 g4 b\rest |
  d b8 g b4. a8 |
  g2. g4^\markup\italic"a tempo" |
  
  e4 d8.[ cis16] d4 b |
  a'4 g8.[ fis16] g8\< fis g a |
  b4\! g8. a16 b4\> b |
  b2.\! a8\mf g |
  e'4 d c e, |
  
  c' b a2 |
  g4\f e8. c'16 b4 g |
  g e8.[ c'16] b2\fermata |
  c4^\markup\italic"a tempo" e,8. e16 f4 e8.[ d16] |
  g2\< a |
  b4(\! d8.)\fermata c16 c2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	It was a Bret -- on vil -- lage,
  That lay by the sea,
  She was a fish -- er -- maid -- en,
  Ma -- rin -- er stout was he;
  Fare -- well true heart, for we must part,
  The winds are call -- ing down the sea,
  But for me thou’lt pray in the chap -- el gray,
  \markup\italic Na -- \markup\italic vi -- \markup\italic tas \markup\italic Sal -- \markup\italic va, \markup\italic Do -- \markup\italic mi -- \markup\italic ne,
  \markup\italic Na -- \markup\italic vi -- \markup\italic tas \markup\italic Sal -- \markup\italic va, \markup\italic Do -- \markup\italic mi -- \markup\italic ne.
  
  \set stanza = #"2. "
  It was a night of ter -- ror,
  Wild, wild was the sea!
  He in the storm is drift -- ing,
  Watch -- ing in prayer is she,
  Watch -- ing in prayer is she,
  Sweet heart! sweet heart! And must we part?
  No boat can live in such a sea,
  But still she cries with stream -- ing eyes,
  \markup\italic Na -- \markup\italic vi -- \markup\italic tas \markup\italic Sal -- \markup\italic va, \markup\italic Do -- \markup\italic mi -- \markup\italic ne,
  \markup\italic Na -- \markup\italic vi -- \markup\italic tas \markup\italic Sal -- \markup\italic va, \markup\italic Do -- \markup\italic mi -- \markup\italic ne!
  
  \set stanza = #"3. "
  Bright was the Bret -- on vil -- lage,
  Bright, bright was the sea,
  She was a fish -- er -- maid -- en,
  Ma -- rin -- er stout was he,
  ’Twas Heav’n a -- bove that saved me, love! and brought me back from the storm to thee,
  In the chap -- el gray
  We’ll kneel and pray,
  \markup\italic Glo -- \markup\italic ri -- \markup\italic a \markup\italic ti -- \markup\italic bi, \markup\italic Do -- \markup\italic mi -- \markup\italic ne,
  \markup\italic Glo -- \markup\italic ri -- \markup\italic a \markup\italic ti -- \markup\italic bi, __ \markup\italic ti -- \markup\italic bi, \markup\italic Do -- \markup\italic mi -- \markup\italic ne!
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
  e4 e8. e16 e4 c |
  c2 c4 s |
  s b d c8 b |
  c2. s4 |
  c c8 c dis4 e8[ fis] |
  g2 e4 s |
  
  g g8 d fis4. d8 |
  d2. d4 |
  c b8[ ais] b4 b |
  e e e8 e e e |
  e4 e fis fis |
  e2. f8 f |
  e4 f g c,8 c |
  e4 d c2 |
  c4 d8 c c4 c |
  b b c2 |
  d4 c8 c a4 a |
  c2( d4) b |
  c2. s4 |
  
  %verse 2(alto)
  c4 c8 c e4 e |
  e2 e4 s |
  e2 f4 e8 d |
  c2. s4 |
  d4 d8 d d4 d |
  
  c2 e4 s |
  dis dis8 dis d4 dis |
  e2. s4 |
  a,4 a8 a a4 b |
  b2. b4 |
  b b b b |
  
  e e e8 e e e |
  e4 e dis dis |
  e2( f4) f |
  e f g c, |
  e e e2 |
  c4 c8 c f4 f |
  
  %page2(alto)
  b,4~ b8 b c2 |
  d4 c8 c c4 c |
  c2( b4) b |
  c2. s4 |
  e4 e8 e e4 c |
  c2 c4 s |
  
  f2 f4 d8 d |
  c2. s4 |
  c c8 c dis4 e8[ fis] |
  g2 e4 s |
  g g8 d fis4. d8 |
  d2. d4 |
  
  b4 b b b |
  e e e8 dis e e |
  e4 e8. e16 dis4 dis |
  e2. f8 f |
  e4 f g c, |
  
  e d c2 |
  e4 c8. e16 d4 d |
  e c8.[ e16] d4( f)\fermata |
  e e8. e16 d4 c |
  c2 f |
  f4~ f8. e16 e2 \bar"|."
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
  c4 c8. c16 a4 a |
  a2 a4 s |
  s g g g8 g |
  g2. s4 |
  g4 g8 g b4 b |
  b2 b4 s4 |
  
  b4 d8 b c4. c8 |
  b2. b4 |
  g g g g |
  g g g8 g g g |
  g4 b b b |
  gis2. b8 b |
  
  c4 b c c8 c |
  a4 gis a2 |
  a4 a8 a f4 f |
  g g g2 |
  a4 g8 g d'4 d |
  e2( b4) f |
  e2. s4 |
  
  %verse2 (tenor)
  a4 a8 a a4 a |
  a2 gis4 s |
  c2 d4 a8 a |
  a2. s4 |
  gis4 gis8 gis gis4 gis |
  
  a2 c4 s |
  a a8 a b4 a |
  gis2. s4 |
  dis4 dis8 dis dis4 a' |
  gis2. g4 |
  g g g g |
  
  g g g8 g g g |
  g4 b fis fis |
  g2. b4 |
  c b c g |
  a gis a2 |
  c4 c8 c c4 c |
  
  %page2 (tenor)
  g4~ g8 g g2 |
  a4 a8 a a4 f |
  e2( d4) f |
  e2. s4 |
  c' c8 c a4 a |
  a2 a4 s |
  
  b2 b4 g8 g |
  g2. s4 |
  g g8 g b4 b |
  b2 b4 s |
  b d8 b d4. c8 |
  b2. b4 |
  
  g f f g |
  g c c8 c c fis, |
  g4 g8. g16 fis4 fis |
  gis2. b8 b |
  c4 b c g |
  
  a gis a2 |
  c4 g8. g16 g4 b |
  c c d2 |
  c4 c8. c16 d4 c8.[ d16] |
  e2 b |
  d4( b8.) g16 g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,4 c8. c16 a'4 a |
  f2 f4 d4\rest |
  d\rest g, g g8 g |
  c2. d4\rest |
  c c8 c b4 b |
  e2 e4 d4\rest |
  
  d d8 d d4. d8 |
  g2. g4 |
  g, g g d' |
  c c c8 c c c |
  e4 e dis dis |
  e2. g8 g |
  
  c,4 d e g8 g |
  a4 e a,2 |
  a4 a8 a f'4 f |
  f f e2 |
  d4 e8 e f4 fis |
  g2. g,4 |
  c2. d4\rest |
  
  a4 a8 a c4 a |
  e'2 e4 d\rest |
  a'2 d,4 d8 d |
  a2. d4\rest |
  e e8 e e4 e |
  
  a2 a4 d,4\rest |
  f f8 f f4 f |
  e2. d4\rest |
  b4 b8 b b4 dis? |
  e2. g,4 |
  g g g g |
  
  c c c8 c c c |
  b4 b b b |
  e2( d4) g |
  c, d e c |
  a b c2 |
  g'4 g8 g f4 f |
  
  %page2 (bass)
  f4~ f8 f e2 |
  d4 e8 e f4 d |
  g,2. g4 |
  c2. d4\rest |
  c4 c8 c a'4 a |
  f2 f4 d\rest |
  
  g2 g4 b,8 b |
  c2. d4\rest |
  c c8 c b4 b |
  e2 e4 d\rest |
  d d8 d d4. d8 |
  g2. g4 |
  
  g, g g g |
  c c c8 c c c |
  b4 b8. b16 b4 b |
  e2. g8 g |
  c,4 d e c |
  
  a e' f2 |
  g4 g8. g16 g4 g |
  c, c g'( gis)\fermata |
  a a8. a16 d,4 g8.[ f16] |
  g2 g |
  g4~ g8. g16 c,2 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Saved from the Storm"}}
  composer = \markup\oldStyleNum"Odoardo Barri (1844–1920)"
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  tagline = ""
}}


