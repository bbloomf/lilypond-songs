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
  first-page-number = #134
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  d4 d8[ e] f[ g] a4 |
  f e d2 |
  f4 e d cis? |
  d8[ e] f4 e2 | \break
  
  d4 d8[ e] f[ g] a4 |
  f e d2 |
  e4 f8[ g] a4 g |
  f e d2 | \break
  
  d4\p c d e |
  f a8[ g] f2 |
  f4\< e f g |
  a\! c8[ b] a2 | \break
  
  d4\f d c a |
  f g a2 |
  d,4 d8[ e] f[ g] a4\> |
  f e\! d2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  a4 d8[ cis] d[ e] f4 |
  d cis a2 |
  d4 cis d a |
  a d cis2 |
  
  a4 a8[ cis] d[ e] f4 |
  d cis a2 |
  cis4 d8[ e] f4 e |
  d cis a2 |
  
  bes4 a bes bes |
  c e c2 |
  d4 cis d e8[ c] |
  c4 e8[ d] c2 |
  
  f4 f8[ e] f4 c |
  d d e2 |
  d4 d8[ cis] d[ e] f4 |
  d cis a2 \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Je -- sus, Lov -- er of my soul,
  Let me to Thy bos -- om fly,
  While the bil -- lows near me roll,
  While the tem -- pest still is high!
  Hide me, O my Sav -- ior, hide,
  Till the storm of life is past;
  Safe in -- to the ha -- ven guide,
  O re -- ceive my soul at last!
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  Oth -- er ref -- uge have I none,
  Hangs my help -- less soul on Thee:
  Leave, ah, leave me not a -- lone,
  Still sup -- port and com -- fort me;
  All my trust on Thee is stayed,
  All my help from Thee I bring;
  Cov -- er my de -- fence -- less head
  With the shad -- ow of Thy wing!
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Thou, oh, Christ, art all I want,
  More than all in Thee I find;
  Raise the fall -- en, cheer the faint,
  Heal the sick, and lead the blind,
  Just and ho -- ly is Thy Name,
  I am all un -- right -- eous -- ness!
  Vile and full of sin I am,
  Thou art full of truth and grace.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  Plen -- teous grace with Thee is found,
  Grace to cov -- er all my sin;
  Let the heal -- ing streams a -- bound;
  Make and keep me pure with -- in.
  Thou of life the foun -- tain art,
  Free -- ly let me take of Thee;
  Spring Thou up with -- in my heart,
  Rise to all e -- ter -- ni -- ty.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  f4 a a a |
  a a8[ g] f2 |
  a4 a a a |
  a a a2 |
  
  f8[ g] a4 a a |
  a e8[ g] f2 |
  a4 a8[ c] c4 bes |
  a a8[ g] f2 |
  
  f4 f f g |
  a c8[ bes] a2 |
  a4 a a c8[ e,] |
  f8( a4) gis8 a2 |
  
  bes4 bes c f, |
  a d cis2 |
  a8[ bes] a[ g] a4 a |
  a e16[ f g8] f2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d4 a d4 f,8[ g] |
  a4 a d2 |
  d4 e f g |
  f8[ e] d4 a2 |
  
  d8[ e] f[ e] d4 f,8[ g] |
  a4 a d2 |
  a4 d8[ c] f4 g |
  a a, d2 |
  
  bes4 f bes8[ a] g4 |
  f c' f,2 |
  d'4 a d c |
  f e a,2 |
  
  bes4 bes' a f |
  d bes a2 |
  f'8[ g] f[ e] d4 f,8[ g] |
  a4 a d2 \bar "|."
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
      \new Voice = "sopranos" \transpose f a { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" \transpose f a { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \lyricsto "altos" \altoWords
    \new Lyrics \lyricsto "altos" \altoWordsII
    \new Lyrics \lyricsto "altos" \altoWordsIII
    \new Lyrics \lyricsto "altos" \altoWordsIV
    \new Lyrics \lyricsto "altos" \altoWordsV
    \new Lyrics \lyricsto "altos" \altoWordsVI
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" \transpose f a { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" \transpose f a { \voiceTwo << \global \bassMusic >> }
    >>
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
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 4)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Jesus, Lover of my soul"}}
  poet = \markup\oldStyleNum"Charles Wesly (1708–1788)"
  composer = \markup\concat{\italic "Aberystwyth" \oldStyleNum", Joseph Parry (1841–1903)"}
  tagline = ""
}}



